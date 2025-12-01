// JIT Runtime - Native module for executing machine code on macOS ARM64
// Extended with primitives for self-hosting compiler

#include <node_api.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <pthread.h>
#include <libkern/OSCacheControl.h>

#ifndef MAP_JIT
#define MAP_JIT 0x0800
#endif

// === Type tags for runtime values ===
// Values are tagged pointers:
// - Integers: value << 1 | 1 (odd = integer)
// - Pointers: value (even = pointer to heap object)
// Heap objects have a type tag in first word

#define TAG_INT      0x01
#define IS_INT(x)    ((x) & 1)
#define MAKE_INT(x)  (((x) << 1) | 1)
#define GET_INT(x)   ((int64_t)(x) >> 1)

#define TYPE_CONS    0x01
#define TYPE_STRING  0x02
#define TYPE_VECTOR  0x03
#define TYPE_CLOSURE 0x04

typedef struct {
    int64_t type;
    int64_t car;
    int64_t cdr;
} ConsCell;

typedef struct {
    int64_t type;
    int64_t length;
    char data[];
} String;

typedef struct {
    int64_t type;
    int64_t length;
    int64_t data[];
} Vector;

// === JIT Memory Management ===
static void* jit_memory = NULL;
static size_t jit_memory_size = 0;

// === Heap for runtime allocations ===
static void* heap_base = NULL;
static size_t heap_size = 0;
static size_t heap_used = 0;

// === Runtime Functions (callable from JIT code) ===

// Print integer
static void rt_print_int(int64_t value) {
    if (IS_INT(value)) {
        printf("%lld\n", GET_INT(value));
    } else {
        printf("%lld\n", value);
    }
}

// Print string
static void rt_print_str(const char* str) {
    printf("%s\n", str);
}

// Print a value (auto-detect type)
static void rt_print(int64_t value) {
    if (IS_INT(value)) {
        printf("%lld\n", GET_INT(value));
    } else if (value == 0) {
        printf("nil\n");
    } else {
        int64_t type = *(int64_t*)value;
        if (type == TYPE_STRING) {
            String* s = (String*)value;
            printf("%s\n", s->data);
        } else if (type == TYPE_CONS) {
            printf("(cons ...)\n");
        } else {
            printf("<object %p>\n", (void*)value);
        }
    }
}

// Allocate memory on heap
static int64_t rt_alloc(int64_t size) {
    if (heap_base == NULL || heap_used + size > heap_size) {
        return 0;  // Out of memory
    }
    void* ptr = (char*)heap_base + heap_used;
    heap_used += (size + 15) & ~15;  // Align to 16 bytes
    return (int64_t)ptr;
}

// Create a cons cell
static int64_t rt_cons(int64_t car, int64_t cdr) {
    ConsCell* cell = (ConsCell*)rt_alloc(sizeof(ConsCell));
    if (!cell) return 0;
    cell->type = TYPE_CONS;
    cell->car = car;
    cell->cdr = cdr;
    return (int64_t)cell;
}

// Get car of cons cell
static int64_t rt_car(int64_t cons) {
    if (cons == 0 || IS_INT(cons)) return 0;
    ConsCell* cell = (ConsCell*)cons;
    if (cell->type != TYPE_CONS) return 0;
    return cell->car;
}

// Get cdr of cons cell
static int64_t rt_cdr(int64_t cons) {
    if (cons == 0 || IS_INT(cons)) return 0;
    ConsCell* cell = (ConsCell*)cons;
    if (cell->type != TYPE_CONS) return 0;
    return cell->cdr;
}

// Check if value is a cons cell
// Note: we use raw integers, so small values are likely integers, not pointers
static int64_t rt_consp(int64_t value) {
    if (value == 0) return 0;
    // Small positive values are likely raw integers, not valid heap pointers
    // Heap pointers are typically large (high memory addresses)
    if (value > 0 && value < 0x10000) return 0;
    // Check if it looks like a valid heap pointer and has cons type
    ConsCell* cell = (ConsCell*)value;
    return (cell->type == TYPE_CONS) ? 1 : 0;
}

// Check if value is null
static int64_t rt_null(int64_t value) {
    return (value == 0) ? 1 : 0;
}

// Create a string from C string
static int64_t rt_make_string(const char* src, int64_t len) {
    String* s = (String*)rt_alloc(sizeof(String) + len + 1);
    if (!s) return 0;
    s->type = TYPE_STRING;
    s->length = len;
    memcpy(s->data, src, len);
    s->data[len] = 0;
    return (int64_t)s;
}

// Get string length
static int64_t rt_string_length(int64_t str) {
    if (str == 0 || IS_INT(str)) return 0;
    String* s = (String*)str;
    if (s->type != TYPE_STRING) return 0;
    return s->length;
}

// Get character at index (idx is raw integer, not tagged)
static int64_t rt_string_ref(int64_t str, int64_t idx) {
    if (str == 0 || IS_INT(str)) return 0;
    String* s = (String*)str;
    if (s->type != TYPE_STRING) return 0;
    // idx is a raw integer, not tagged
    if (idx < 0 || idx >= s->length) return 0;
    return (int64_t)(unsigned char)s->data[idx];
}

// Get raw string pointer
static const char* rt_string_data(int64_t str) {
    if (str == 0 || IS_INT(str)) return "";
    String* s = (String*)str;
    if (s->type != TYPE_STRING) return "";
    return s->data;
}

// Substring (start/end are raw integers, not tagged)
static int64_t rt_substring(int64_t str, int64_t start, int64_t end) {
    if (str == 0 || IS_INT(str)) return 0;
    String* s = (String*)str;
    if (s->type != TYPE_STRING) return 0;
    // start/end are raw integers, not tagged
    if (start < 0) start = 0;
    if (end > s->length) end = s->length;
    if (start >= end) return rt_make_string("", 0);
    return rt_make_string(s->data + start, end - start);
}

// String concatenation
static int64_t rt_string_append(int64_t s1, int64_t s2) {
    const char* a = rt_string_data(s1);
    const char* b = rt_string_data(s2);
    int64_t len1 = strlen(a);
    int64_t len2 = strlen(b);
    String* s = (String*)rt_alloc(sizeof(String) + len1 + len2 + 1);
    if (!s) return 0;
    s->type = TYPE_STRING;
    s->length = len1 + len2;
    memcpy(s->data, a, len1);
    memcpy(s->data + len1, b, len2);
    s->data[len1 + len2] = 0;
    return (int64_t)s;
}

// String equality
static int64_t rt_string_equal(int64_t s1, int64_t s2) {
    const char* a = rt_string_data(s1);
    const char* b = rt_string_data(s2);
    return strcmp(a, b) == 0 ? 1 : 0;
}

// Integer to string (value is raw integer)
static int64_t rt_int_to_string(int64_t value) {
    char buf[32];
    snprintf(buf, sizeof(buf), "%lld", value);
    return rt_make_string(buf, strlen(buf));
}

// String to integer
static int64_t rt_string_to_int(int64_t str) {
    const char* s = rt_string_data(str);
    return atoll(s);
}

// Make a single-character string from char code
static int64_t rt_make_char_string(int64_t charcode) {
    char buf[2];
    buf[0] = (char)charcode;
    buf[1] = 0;
    return rt_make_string(buf, 1);
}

// Character predicates (c is raw char code)
static int64_t rt_char_whitespace(int64_t c) {
    return (c == ' ' || c == '\t' || c == '\n' || c == '\r') ? 1 : 0;
}

static int64_t rt_char_digit(int64_t c) {
    return (c >= '0' && c <= '9') ? 1 : 0;
}

static int64_t rt_char_alpha(int64_t c) {
    return ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) ? 1 : 0;
}

// Create vector (size is raw integer)
static int64_t rt_make_vector(int64_t size) {
    Vector* v = (Vector*)rt_alloc(sizeof(Vector) + size * sizeof(int64_t));
    if (!v) return 0;
    v->type = TYPE_VECTOR;
    v->length = size;
    for (int64_t i = 0; i < size; i++) v->data[i] = 0;
    return (int64_t)v;
}

// Vector ref (idx is raw integer)
static int64_t rt_vector_ref(int64_t vec, int64_t idx) {
    if (vec == 0 || IS_INT(vec)) return 0;
    Vector* v = (Vector*)vec;
    if (v->type != TYPE_VECTOR) return 0;
    if (idx < 0 || idx >= v->length) return 0;
    return v->data[idx];
}

// Vector set (idx is raw integer)
static void rt_vector_set(int64_t vec, int64_t idx, int64_t val) {
    if (vec == 0 || IS_INT(vec)) return;
    Vector* v = (Vector*)vec;
    if (v->type != TYPE_VECTOR) return;
    if (idx < 0 || idx >= v->length) return;
    v->data[idx] = val;
}

// Vector length
static int64_t rt_vector_length(int64_t vec) {
    if (vec == 0 || IS_INT(vec)) return 0;
    Vector* v = (Vector*)vec;
    if (v->type != TYPE_VECTOR) return 0;
    return v->length;
}

// Read entire file
static int64_t rt_read_file(int64_t filename) {
    const char* fname = rt_string_data(filename);
    FILE* f = fopen(fname, "rb");
    if (!f) return 0;
    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fseek(f, 0, SEEK_SET);
    String* s = (String*)rt_alloc(sizeof(String) + size + 1);
    if (!s) { fclose(f); return 0; }
    s->type = TYPE_STRING;
    s->length = size;
    fread(s->data, 1, size, f);
    s->data[size] = 0;
    fclose(f);
    return (int64_t)s;
}

// Write to file
static int64_t rt_write_file(int64_t filename, int64_t content) {
    const char* fname = rt_string_data(filename);
    const char* data = rt_string_data(content);
    FILE* f = fopen(fname, "wb");
    if (!f) return 0;
    int64_t len = rt_string_length(content);
    fwrite(data, 1, len, f);
    fclose(f);
    return 1;
}

// Error/exit
static void rt_error(int64_t msg) {
    const char* s = rt_string_data(msg);
    fprintf(stderr, "Error: %s\n", s);
    exit(1);
}

// === Bitwise Operations ===
// These are essential for the code generator to emit machine code

static int64_t rt_bitand(int64_t a, int64_t b) {
    return a & b;
}

static int64_t rt_bitor(int64_t a, int64_t b) {
    return a | b;
}

static int64_t rt_bitxor(int64_t a, int64_t b) {
    return a ^ b;
}

static int64_t rt_bitnot(int64_t a) {
    return ~a;
}

// Logical shift left
static int64_t rt_lsl(int64_t a, int64_t b) {
    return a << b;
}

// Arithmetic shift right
static int64_t rt_asr(int64_t a, int64_t b) {
    return a >> b;
}

// Logical shift right (for unsigned operations)
static int64_t rt_lsr(int64_t a, int64_t b) {
    return (int64_t)((uint64_t)a >> b);
}

// === Node.js N-API Bindings ===

static napi_value AllocateMemory(napi_env env, napi_callback_info info) {
    size_t argc = 1;
    napi_value args[1];
    napi_get_cb_info(env, info, &argc, args, NULL, NULL);

    int64_t size;
    napi_get_value_int64(env, args[0], &size);

    if (jit_memory != NULL) {
        munmap(jit_memory, jit_memory_size);
    }

    jit_memory = mmap(NULL, size, PROT_READ | PROT_WRITE | PROT_EXEC,
                      MAP_PRIVATE | MAP_ANONYMOUS | MAP_JIT, -1, 0);

    if (jit_memory == MAP_FAILED) {
        jit_memory = NULL;
        napi_throw_error(env, NULL, "Failed to allocate JIT memory");
        return NULL;
    }

    jit_memory_size = size;

    napi_value result;
    napi_get_boolean(env, true, &result);
    return result;
}

static napi_value AllocateHeap(napi_env env, napi_callback_info info) {
    size_t argc = 1;
    napi_value args[1];
    napi_get_cb_info(env, info, &argc, args, NULL, NULL);

    int64_t size;
    napi_get_value_int64(env, args[0], &size);

    if (heap_base != NULL) {
        free(heap_base);
    }

    heap_base = malloc(size);
    heap_size = size;
    heap_used = 0;

    napi_value result;
    napi_get_boolean(env, heap_base != NULL, &result);
    return result;
}

static napi_value WriteCode(napi_env env, napi_callback_info info) {
    size_t argc = 1;
    napi_value args[1];
    napi_get_cb_info(env, info, &argc, args, NULL, NULL);

    if (jit_memory == NULL) {
        napi_throw_error(env, NULL, "JIT memory not allocated");
        return NULL;
    }

    void* data;
    size_t length;
    napi_get_buffer_info(env, args[0], &data, &length);

    if (length > jit_memory_size) {
        napi_throw_error(env, NULL, "Code too large for allocated memory");
        return NULL;
    }

    pthread_jit_write_protect_np(0);
    memcpy(jit_memory, data, length);
    sys_icache_invalidate(jit_memory, length);
    pthread_jit_write_protect_np(1);

    napi_value result;
    napi_get_boolean(env, true, &result);
    return result;
}

typedef int64_t (*JitFunc)(void);

static napi_value Execute(napi_env env, napi_callback_info info) {
    if (jit_memory == NULL) {
        napi_throw_error(env, NULL, "JIT memory not allocated");
        return NULL;
    }

    JitFunc func = (JitFunc)jit_memory;
    int64_t result = func();

    napi_value js_result;
    napi_create_int64(env, result, &js_result);
    return js_result;
}

// Macro to create address getter
#define ADDR_GETTER(name, func) \
static napi_value Get_##name(napi_env env, napi_callback_info info) { \
    napi_value result; \
    napi_create_bigint_uint64(env, (uint64_t)(func), &result); \
    return result; \
}

ADDR_GETTER(print_int, rt_print_int)
ADDR_GETTER(print_str, rt_print_str)
ADDR_GETTER(print, rt_print)
ADDR_GETTER(alloc, rt_alloc)
ADDR_GETTER(cons, rt_cons)
ADDR_GETTER(car, rt_car)
ADDR_GETTER(cdr, rt_cdr)
ADDR_GETTER(consp, rt_consp)
ADDR_GETTER(null, rt_null)
ADDR_GETTER(make_string, rt_make_string)
ADDR_GETTER(string_length, rt_string_length)
ADDR_GETTER(string_ref, rt_string_ref)
ADDR_GETTER(string_data, rt_string_data)
ADDR_GETTER(substring, rt_substring)
ADDR_GETTER(string_append, rt_string_append)
ADDR_GETTER(string_equal, rt_string_equal)
ADDR_GETTER(int_to_string, rt_int_to_string)
ADDR_GETTER(string_to_int, rt_string_to_int)
ADDR_GETTER(make_char_string, rt_make_char_string)
ADDR_GETTER(char_whitespace, rt_char_whitespace)
ADDR_GETTER(char_digit, rt_char_digit)
ADDR_GETTER(char_alpha, rt_char_alpha)
ADDR_GETTER(make_vector, rt_make_vector)
ADDR_GETTER(vector_ref, rt_vector_ref)
ADDR_GETTER(vector_set, rt_vector_set)
ADDR_GETTER(vector_length, rt_vector_length)
ADDR_GETTER(read_file, rt_read_file)
ADDR_GETTER(write_file, rt_write_file)
ADDR_GETTER(error, rt_error)
ADDR_GETTER(bitand, rt_bitand)
ADDR_GETTER(bitor, rt_bitor)
ADDR_GETTER(bitxor, rt_bitxor)
ADDR_GETTER(bitnot, rt_bitnot)
ADDR_GETTER(lsl, rt_lsl)
ADDR_GETTER(asr, rt_asr)
ADDR_GETTER(lsr, rt_lsr)

static napi_value FreeMemory(napi_env env, napi_callback_info info) {
    if (jit_memory != NULL) {
        munmap(jit_memory, jit_memory_size);
        jit_memory = NULL;
        jit_memory_size = 0;
    }
    if (heap_base != NULL) {
        free(heap_base);
        heap_base = NULL;
        heap_size = 0;
        heap_used = 0;
    }

    napi_value result;
    napi_get_boolean(env, true, &result);
    return result;
}

static napi_value Init(napi_env env, napi_value exports) {
    napi_property_descriptor props[] = {
        { "allocateMemory", NULL, AllocateMemory, NULL, NULL, NULL, napi_default, NULL },
        { "allocateHeap", NULL, AllocateHeap, NULL, NULL, NULL, napi_default, NULL },
        { "writeCode", NULL, WriteCode, NULL, NULL, NULL, napi_default, NULL },
        { "execute", NULL, Execute, NULL, NULL, NULL, napi_default, NULL },
        { "freeMemory", NULL, FreeMemory, NULL, NULL, NULL, napi_default, NULL },
        { "getPrintIntAddr", NULL, Get_print_int, NULL, NULL, NULL, napi_default, NULL },
        { "getPrintStrAddr", NULL, Get_print_str, NULL, NULL, NULL, napi_default, NULL },
        { "getPrintAddr", NULL, Get_print, NULL, NULL, NULL, napi_default, NULL },
        { "getAllocAddr", NULL, Get_alloc, NULL, NULL, NULL, napi_default, NULL },
        { "getConsAddr", NULL, Get_cons, NULL, NULL, NULL, napi_default, NULL },
        { "getCarAddr", NULL, Get_car, NULL, NULL, NULL, napi_default, NULL },
        { "getCdrAddr", NULL, Get_cdr, NULL, NULL, NULL, napi_default, NULL },
        { "getConspAddr", NULL, Get_consp, NULL, NULL, NULL, napi_default, NULL },
        { "getNullAddr", NULL, Get_null, NULL, NULL, NULL, napi_default, NULL },
        { "getMakeStringAddr", NULL, Get_make_string, NULL, NULL, NULL, napi_default, NULL },
        { "getStringLengthAddr", NULL, Get_string_length, NULL, NULL, NULL, napi_default, NULL },
        { "getStringRefAddr", NULL, Get_string_ref, NULL, NULL, NULL, napi_default, NULL },
        { "getStringDataAddr", NULL, Get_string_data, NULL, NULL, NULL, napi_default, NULL },
        { "getSubstringAddr", NULL, Get_substring, NULL, NULL, NULL, napi_default, NULL },
        { "getStringAppendAddr", NULL, Get_string_append, NULL, NULL, NULL, napi_default, NULL },
        { "getStringEqualAddr", NULL, Get_string_equal, NULL, NULL, NULL, napi_default, NULL },
        { "getIntToStringAddr", NULL, Get_int_to_string, NULL, NULL, NULL, napi_default, NULL },
        { "getStringToIntAddr", NULL, Get_string_to_int, NULL, NULL, NULL, napi_default, NULL },
        { "getMakeCharStringAddr", NULL, Get_make_char_string, NULL, NULL, NULL, napi_default, NULL },
        { "getCharWhitespaceAddr", NULL, Get_char_whitespace, NULL, NULL, NULL, napi_default, NULL },
        { "getCharDigitAddr", NULL, Get_char_digit, NULL, NULL, NULL, napi_default, NULL },
        { "getCharAlphaAddr", NULL, Get_char_alpha, NULL, NULL, NULL, napi_default, NULL },
        { "getMakeVectorAddr", NULL, Get_make_vector, NULL, NULL, NULL, napi_default, NULL },
        { "getVectorRefAddr", NULL, Get_vector_ref, NULL, NULL, NULL, napi_default, NULL },
        { "getVectorSetAddr", NULL, Get_vector_set, NULL, NULL, NULL, napi_default, NULL },
        { "getVectorLengthAddr", NULL, Get_vector_length, NULL, NULL, NULL, napi_default, NULL },
        { "getReadFileAddr", NULL, Get_read_file, NULL, NULL, NULL, napi_default, NULL },
        { "getWriteFileAddr", NULL, Get_write_file, NULL, NULL, NULL, napi_default, NULL },
        { "getErrorAddr", NULL, Get_error, NULL, NULL, NULL, napi_default, NULL },
        { "getBitandAddr", NULL, Get_bitand, NULL, NULL, NULL, napi_default, NULL },
        { "getBitorAddr", NULL, Get_bitor, NULL, NULL, NULL, napi_default, NULL },
        { "getBitxorAddr", NULL, Get_bitxor, NULL, NULL, NULL, napi_default, NULL },
        { "getBitnotAddr", NULL, Get_bitnot, NULL, NULL, NULL, napi_default, NULL },
        { "getLslAddr", NULL, Get_lsl, NULL, NULL, NULL, napi_default, NULL },
        { "getAsrAddr", NULL, Get_asr, NULL, NULL, NULL, napi_default, NULL },
        { "getLsrAddr", NULL, Get_lsr, NULL, NULL, NULL, napi_default, NULL },
    };

    napi_define_properties(env, exports, sizeof(props) / sizeof(props[0]), props);
    return exports;
}

NAPI_MODULE(NODE_GYP_MODULE_NAME, Init)
