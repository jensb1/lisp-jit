# Lisp JIT Compiler for ARM64 macOS

A Lisp compiler and JIT (Just-In-Time) execution environment targeting Apple Silicon (ARM64) Macs. This project implements a complete Lisp system from lexer to native code generation, including a self-hosting compiler written entirely in Lisp.

## Features

- **Ahead-of-Time (AOT) Compiler**: Compiles Lisp source code to ARM64 assembly, then links to create native executables
- **JIT Compiler & REPL**: Interactive environment with real-time compilation and execution
- **Self-Hosting Compiler**: A complete Lisp compiler written in Lisp that generates Mach-O object files
- **Native ARM64 Code Generation**: Direct machine code emission without intermediate representations
- **Rich Runtime**: Built-in support for lists, strings, vectors, and I/O operations

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                        Lisp Source Code                         │
└─────────────────────────────────────────────────────────────────┘
                                │
                                ▼
┌─────────────────────────────────────────────────────────────────┐
│                      Lexer (src/lexer.ts)                       │
│            Tokenizes source into LPAREN, RPAREN,                │
│                NUMBER, SYMBOL, STRING, EOF                      │
└─────────────────────────────────────────────────────────────────┘
                                │
                                ▼
┌─────────────────────────────────────────────────────────────────┐
│                     Parser (src/parser.ts)                      │
│              Builds AST: NumberNode, SymbolNode,                │
│                    StringNode, ListNode                         │
└─────────────────────────────────────────────────────────────────┘
                                │
                ┌───────────────┼───────────────┐
                ▼               ▼               ▼
┌─────────────────────┐ ┌─────────────────┐ ┌─────────────────────┐
│   AOT Compiler      │ │  JIT Compiler   │ │  Self-Hosting       │
│  (src/codegen.ts)   │ │(src/jit-codegen)│ │(lisp/self-hosting-  │
│                     │ │                 │ │    compiler.lisp)   │
│  Generates ARM64    │ │ Generates ARM64 │ │                     │
│  assembly text      │ │ machine code    │ │ Generates Mach-O    │
│                     │ │ in memory       │ │ object files        │
└─────────────────────┘ └─────────────────┘ └─────────────────────┘
         │                      │                      │
         ▼                      ▼                      ▼
┌─────────────────────┐ ┌─────────────────┐ ┌─────────────────────┐
│   as + ld           │ │  Native JIT     │ │   ld (linker)       │
│   (system tools)    │ │  Runtime        │ │                     │
└─────────────────────┘ └─────────────────┘ └─────────────────────┘
         │                      │                      │
         ▼                      ▼                      ▼
┌─────────────────────────────────────────────────────────────────┐
│                    Native ARM64 Executable                      │
└─────────────────────────────────────────────────────────────────┘
```

## Requirements

- **macOS** on Apple Silicon (M1/M2/M3)
- **Node.js** 18+
- **Xcode Command Line Tools** (for `as`, `ld`, and system libraries)

## Installation

```bash
# Clone the repository
git clone https://github.com/jensb1/lisp-jit.git
cd lisp-jit

# Install dependencies
npm install

# Build TypeScript
npm run build

# Build native JIT runtime (optional, for JIT mode)
npm run build:native

# Or build everything at once
npm run build:all
```

## Usage

### AOT Compiler

Compile a Lisp file to a native executable:

```bash
# Compile
npm run compile -- examples/factorial.lisp

# Run the generated executable
./examples/factorial
```

Or use the compiled binary directly:

```bash
node dist/main.js examples/fibonacci.lisp
./examples/fibonacci
```

### JIT REPL

Start an interactive Lisp session:

```bash
npm run repl
# or
npm run jit
```

Example session:

```lisp
Lisp JIT REPL
Type expressions to evaluate. Ctrl+D to exit.

> (+ 1 2 3)
=> 6
> (define (square x) (* x x))
> (square 5)
=> 25
> (define (factorial n)
...   (if (<= n 1)
...       1
...       (* n (factorial (- n 1)))))
> (factorial 10)
=> 3628800
```

### Run a File with JIT

```bash
node dist/repl.js examples/factorial.lisp
```

## Language Reference

### Data Types

| Type | Examples | Description |
|------|----------|-------------|
| Number | `42`, `-17`, `0` | 64-bit signed integers |
| Symbol | `foo`, `my-var`, `+` | Identifiers and operators |
| String | `"hello"`, `"world\n"` | UTF-8 strings with escape sequences |
| List | `(1 2 3)`, `()` | Linked lists (cons cells) |
| Vector | Created via `make-vector` | Mutable arrays |

### Special Forms

#### `define` - Define functions and variables

```lisp
; Define a variable
(define x 42)

; Define a function
(define (add a b) (+ a b))

; Function with multiple body expressions
(define (greet name)
  (print "Hello, ")
  (print name))
```

#### `if` - Conditional expression

```lisp
(if condition
    then-expr
    else-expr)

; else is optional
(if (> x 0)
    (print "positive"))
```

#### `let` - Local bindings

```lisp
(let ((x 10)
      (y 20))
  (+ x y))  ; => 30
```

#### `begin` - Sequence expressions

```lisp
(begin
  (print "first")
  (print "second")
  42)  ; returns 42
```

### Arithmetic Operations

```lisp
(+ 1 2 3)      ; => 6 (variadic)
(- 10 3)       ; => 7
(- 5)          ; => -5 (negation)
(* 2 3 4)      ; => 24 (variadic)
(/ 10 3)       ; => 3 (integer division)
(mod 10 3)     ; => 1 (modulo)
```

### Comparison Operations

```lisp
(= 1 1)        ; => 1 (true)
(< 1 2)        ; => 1
(> 2 1)        ; => 1
(<= 1 1)       ; => 1
(>= 2 1)       ; => 1
```

### Logical Operations

```lisp
(and expr1 expr2 ...)  ; short-circuit AND
(or expr1 expr2 ...)   ; short-circuit OR
(not expr)             ; logical NOT
```

### List Operations

```lisp
(cons 1 (cons 2 (cons 3 (list))))  ; => (1 2 3)
(list 1 2 3)                        ; => (1 2 3)
(car (list 1 2 3))                  ; => 1
(cdr (list 1 2 3))                  ; => (2 3)
(null? (list))                      ; => 1 (true)
(pair? (list 1))                    ; => 1 (true)
```

### String Operations

```lisp
(string-length "hello")              ; => 5
(string-ref "hello" 0)               ; => 104 (ASCII 'h')
(substring "hello" 1 4)              ; => "ell"
(string-append "hello" " " "world")  ; => "hello world"
(string=? "foo" "foo")               ; => 1 (true)
(number->string 42)                  ; => "42"
(string->number "42")                ; => 42
(make-char-string 65)                ; => "A"
```

### Character Predicates

```lisp
(char-whitespace? 32)   ; => 1 (space is whitespace)
(char-numeric? 48)      ; => 1 ('0' is numeric)
(char-alphabetic? 65)   ; => 1 ('A' is alphabetic)
```

### Vector Operations

```lisp
(define v (make-vector 10))   ; Create vector of size 10
(vector-set! v 0 42)          ; Set element at index 0
(vector-ref v 0)              ; => 42
(vector-length v)             ; => 10
```

### Bitwise Operations

```lisp
(bitand 0xFF 0x0F)   ; => 15
(bitor 0xF0 0x0F)    ; => 255
(bitxor 0xFF 0x0F)   ; => 240
(bitnot 0)           ; => -1
(lsl 1 4)            ; => 16 (left shift)
(lsr 16 4)           ; => 1 (logical right shift)
(asr -16 4)          ; => -1 (arithmetic right shift)
```

### I/O Operations

```lisp
(print expr)                ; Print to stdout with newline
(read-file "path")          ; Read file contents as string
(write-file "path" "data")  ; Write string to file
(error "message")           ; Print error and exit
```

### Low-Level I/O (Self-hosting compiler)

```lisp
(write fd buffer length)  ; Write bytes (syscall)
(read fd buffer length)   ; Read bytes (syscall)
(write-byte fd byte)      ; Write single byte
(read-byte fd)            ; Read single byte (-1 on EOF)
(exit code)               ; Exit with status code
```

## Examples

### Factorial

```lisp
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(print (factorial 10))  ; => 3628800
```

### Fibonacci

```lisp
(define (fib n)
  (if (<= n 1)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(print (fib 20))  ; => 6765
```

### FizzBuzz

```lisp
(define (fizzbuzz n)
  (if (= (mod n 15) 0)
      (print "FizzBuzz")
      (if (= (mod n 3) 0)
          (print "Fizz")
          (if (= (mod n 5) 0)
              (print "Buzz")
              (print n)))))

(define (loop i max)
  (if (<= i max)
      (begin
        (fizzbuzz i)
        (loop (+ i 1) max))))

(loop 1 100)
```

### Working with Lists

```lisp
(define (length lst)
  (if (null? lst)
      0
      (+ 1 (length (cdr lst)))))

(define (reverse-helper lst acc)
  (if (null? lst)
      acc
      (reverse-helper (cdr lst) (cons (car lst) acc))))

(define (reverse lst)
  (reverse-helper lst (list)))

(define (map f lst)
  (if (null? lst)
      (list)
      (cons (f (car lst)) (map f (cdr lst)))))

(define nums (list 1 2 3 4 5))
(print (length nums))              ; => 5
(print (length (reverse nums)))    ; => 5
```

## Project Structure

```
lisp-jit/
├── src/
│   ├── main.ts           # AOT compiler entry point
│   ├── repl.ts           # JIT REPL entry point
│   ├── lexer.ts          # Tokenizer
│   ├── parser.ts         # AST builder
│   ├── codegen.ts        # AOT code generator (ARM64 assembly)
│   ├── jit-codegen.ts    # JIT code generator (machine code)
│   ├── jit.ts            # JIT executor wrapper
│   ├── arm64-assembler.ts # ARM64 assembler utilities
│   ├── jit-runtime.c     # Native JIT runtime (C)
│   └── bootstrap-driver.ts # Bootstrap utilities
├── lisp/
│   ├── self-hosting-compiler.lisp  # Self-hosting compiler
│   ├── lexer.lisp        # Lisp lexer in Lisp
│   ├── parser.lisp       # Lisp parser in Lisp
│   ├── codegen.lisp      # Code generation in Lisp
│   ├── macho-generator.lisp # Mach-O object file generator
│   └── *.lisp            # Various test and utility files
├── examples/
│   ├── factorial.lisp    # Factorial example
│   ├── fibonacci.lisp    # Fibonacci example
│   └── *.lisp            # More examples
├── package.json
├── tsconfig.json
├── binding.gyp           # Native module build config
└── build.sh              # Build script
```

## Self-Hosting Compiler

The project includes a self-hosting compiler (`lisp/self-hosting-compiler.lisp`) written entirely in Lisp. This compiler:

1. **Tokenizes** Lisp source code into tokens
2. **Parses** tokens into an AST
3. **Generates** ARM64 machine code
4. **Emits** Mach-O object files

The self-hosting compiler can compile Lisp programs to object files that can be linked with the system linker:

```bash
# Run the self-hosting compiler (outputs object file bytes)
node dist/repl.js lisp/self-hosting-compiler.lisp > output.bytes

# Convert bytes to object file and link
# (This requires additional tooling to convert the byte output)
```

### Bootstrap Process

The bootstrap process works as follows:

1. **Stage 0**: TypeScript compiler (`src/`) compiles Lisp code
2. **Stage 1**: JIT executes the self-hosting compiler written in Lisp
3. **Stage 2**: Self-hosting compiler generates Mach-O object files
4. **Stage 3**: System linker creates native executables

## Technical Details

### ARM64 Code Generation

The compiler generates ARM64 machine code following the Apple ARM64 ABI:

- **Registers**: X0-X7 for arguments/return, X9-X15 for temporaries, X19-X28 callee-saved
- **Stack**: 16-byte aligned, grows downward
- **Calling Convention**: Standard ARM64 procedure call standard

### Memory Management

- **JIT Mode**: Uses a simple bump allocator with 64MB heap
- **AOT Mode**: Stack-based allocation, no dynamic heap
- **Self-Hosting**: Stack-allocated heap region for vectors

### Mach-O Format

The self-hosting compiler generates Mach-O object files with:

- `MH_OBJECT` file type
- `__TEXT,__text` section for code
- Symbol table with `_main` entry point
- `LC_SEGMENT_64`, `LC_SYMTAB`, and `LC_BUILD_VERSION` load commands

## Development

### Building

```bash
# Full rebuild
npm run build:all

# TypeScript only
npm run build

# Native module only
npm run build:native
```

### Testing

```bash
# Run an example
node dist/main.js examples/factorial.lisp
./examples/factorial

# Test JIT
node dist/repl.js examples/fibonacci.lisp
```

## Limitations

- **ARM64 macOS only**: No support for x86 or other platforms
- **Integer only**: No floating-point support
- **No garbage collection**: Memory is not reclaimed in JIT mode
- **No tail-call optimization**: Deep recursion will overflow stack
- **Limited error messages**: Parser and compiler errors are basic

## License

ISC License

## Acknowledgments

- Built for Apple Silicon Macs
- Inspired by classic Lisp implementations and modern JIT compilers
