/*
 * Native Loader for Self-Hosting Lisp Compiler
 * Loads raw ARM64 machine code and executes it directly
 * No TypeScript/Node.js required
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <pthread.h>

// Read bytes from stdin (one number per line)
// First line is count, then count lines of byte values
static unsigned char* read_code(size_t* out_size) {
    char line[256];

    // Read byte count
    if (!fgets(line, sizeof(line), stdin)) {
        fprintf(stderr, "Error: Expected byte count\n");
        return NULL;
    }

    size_t count = (size_t)atol(line);
    if (count == 0 || count > 1024*1024) {
        fprintf(stderr, "Error: Invalid byte count: %zu\n", count);
        return NULL;
    }

    unsigned char* code = malloc(count);
    if (!code) {
        fprintf(stderr, "Error: Failed to allocate %zu bytes\n", count);
        return NULL;
    }

    for (size_t i = 0; i < count; i++) {
        if (!fgets(line, sizeof(line), stdin)) {
            fprintf(stderr, "Error: Unexpected end of input at byte %zu\n", i);
            free(code);
            return NULL;
        }
        code[i] = (unsigned char)atoi(line);
    }

    *out_size = count;
    return code;
}

int main(int argc, char** argv) {
    size_t code_size;
    unsigned char* code = read_code(&code_size);
    if (!code) {
        return 1;
    }

    // Allocate executable memory
    // Round up to page size
    size_t page_size = 4096;
    size_t alloc_size = (code_size + page_size - 1) & ~(page_size - 1);

    void* mem = mmap(NULL, alloc_size,
                     PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANONYMOUS | MAP_JIT,
                     -1, 0);

    if (mem == MAP_FAILED) {
        fprintf(stderr, "Error: mmap failed\n");
        free(code);
        return 1;
    }

    // Copy code
    pthread_jit_write_protect_np(0);  // Disable write protection
    memcpy(mem, code, code_size);
    pthread_jit_write_protect_np(1);  // Re-enable write protection

    // Make executable
    if (mprotect(mem, alloc_size, PROT_READ | PROT_EXEC) != 0) {
        fprintf(stderr, "Error: mprotect failed\n");
        munmap(mem, alloc_size);
        free(code);
        return 1;
    }

    // Clear instruction cache
    __builtin___clear_cache(mem, (char*)mem + code_size);

    free(code);

    // Execute - the code will call exit() syscall directly
    typedef void (*entry_fn)(void);
    entry_fn fn = (entry_fn)mem;
    fn();

    // If we get here, the code didn't call exit
    munmap(mem, alloc_size);
    return 0;
}
