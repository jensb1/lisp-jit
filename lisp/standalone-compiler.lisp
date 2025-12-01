; Standalone Self-Hosting Lisp Compiler
; Generates completely self-contained ARM64 executables for macOS
; No TypeScript runtime required - uses direct syscalls

; ===== Register Constants =====
(define X0 0) (define X1 1) (define X2 2) (define X3 3)
(define X4 4) (define X5 5) (define X6 6) (define X7 7)
(define X8 8) (define X9 9) (define X10 10) (define X11 11)
(define X12 12) (define X13 13) (define X14 14) (define X15 15)
(define X16 16) (define X17 17) (define X19 19) (define X20 20)
(define X21 21) (define X22 22) (define X23 23) (define X24 24)
(define X28 28) (define X29 29) (define X30 30)
(define SP 31) (define XZR 31)

; Condition codes
(define COND-EQ 0) (define COND-NE 1)
(define COND-GE 10) (define COND-LT 11)
(define COND-GT 12) (define COND-LE 13)

; macOS ARM64 syscall numbers
(define SYS-EXIT 1)
(define SYS-READ 3)
(define SYS-WRITE 4)
(define SYS-OPEN 5)
(define SYS-CLOSE 6)
(define SYS-MMAP 197)

; ARM64 instruction bases (decimal)
(define MOVZ-BASE 3531603968)      ; 0xD2800000
(define MOVN-BASE 2457862144)      ; 0x92800000
(define MOVK-BASE 4068474880)      ; 0xF2800000
(define ORR-BASE 2852127712)       ; 0xAA0003E0
(define AND-BASE 2315255808)       ; 0x8A000000
(define EOR-BASE 3388997632)       ; 0xCA000000
(define ORR-IMM-BASE 2985099264)   ; 0xB2000000
(define ADD-REG-BASE 2332033024)   ; 0x8B000000
(define ADD-IMM-BASE 2432696320)   ; 0x91000000
(define SUB-REG-BASE 3405774848)   ; 0xCB000000
(define SUB-IMM-BASE 3506438144)   ; 0xD1000000
(define MUL-BASE 2600500224)       ; 0x9B007C00
(define SDIV-BASE 2596277248)      ; 0x9AC00C00
(define UDIV-BASE 2596276224)      ; 0x9AC00800
(define MSUB-BASE 2600501248)      ; 0x9B008000
(define LSL-REG-BASE 2596274176)   ; 0x9AC02000
(define LSR-REG-BASE 2596274432)   ; 0x9AC02400
(define ASR-REG-BASE 2596274688)   ; 0x9AC02800
(define SUBS-REG-BASE 3942645760)  ; 0xEB000000
(define SUBS-IMM-BASE 4043309056)  ; 0xF1000000
(define CSINC-BASE 2594113536)     ; 0x9A9F07E0
(define CSEL-BASE 2594111488)      ; 0x9A800000
(define LDR-IMM-BASE 4181721088)   ; 0xF9400000
(define STR-IMM-BASE 4177526784)   ; 0xF9000000
(define LDRB-IMM-BASE 956301312)   ; 0x39400000
(define STRB-IMM-BASE 939524096)   ; 0x38000000
(define LDR-POST-BASE 4164944896)  ; 0xF8400400
(define STR-PRE-BASE 4160752640)   ; 0xF8000C00
(define STP-PRE-BASE 2843738112)   ; 0xA9800000
(define LDP-POST-BASE 2831155200)  ; 0xA8C00000
(define B-BASE 335544320)          ; 0x14000000
(define BL-BASE 2483027968)        ; 0x94000000
(define BR-BASE 3594387456)        ; 0xD61F0000
(define BLR-BASE 3594452992)       ; 0xD63F0000
(define RET-INSTR 3596551104)      ; 0xD65F03C0
(define BCOND-BASE 1409286144)     ; 0x54000000
(define SVC-BASE 3556769792)       ; 0xD4000001 (SVC #0)
(define NOP-INSTR 3573751839)      ; 0xD503201F

; Masks
(define MASK-16BIT 65535)
(define MASK-12BIT 4095)
(define MASK-9BIT 511)
(define MASK-7BIT 127)
(define MASK-26BIT 67108863)
(define MASK-19BIT 524287)
(define MASK-6BIT 63)

; ===== Utility Functions =====
(define (list-length lst) (if (null? lst) 0 (+ 1 (list-length (cdr lst)))))
(define (list-ref lst n) (if (= n 0) (car lst) (list-ref (cdr lst) (- n 1))))
(define (reverse-h lst acc) (if (null? lst) acc (reverse-h (cdr lst) (cons (car lst) acc))))
(define (reverse lst) (reverse-h lst (list)))
(define (append-h lst1 lst2) (if (null? lst1) lst2 (cons (car lst1) (append-h (cdr lst1) lst2))))
(define (append lst1 lst2) (append-h lst1 lst2))

; ===== Assembler =====
(define (make-asm)
  (let ((s (make-vector 6)))
    (vector-set! s 0 (make-vector 65536))  ; code buffer
    (vector-set! s 1 0)                     ; code position
    (vector-set! s 2 (list))                ; labels
    (vector-set! s 3 (list))                ; fixups
    (vector-set! s 4 0)                     ; label counter
    (vector-set! s 5 (make-vector 8192))    ; data section
    s))

(define (asm-buf a) (vector-ref a 0))
(define (asm-pos a) (vector-ref a 1))
(define (asm-labels a) (vector-ref a 2))
(define (asm-fixups a) (vector-ref a 3))
(define (asm-ctr a) (vector-ref a 4))
(define (asm-data a) (vector-ref a 5))
(define (asm-set-pos! a p) (vector-set! a 1 p))
(define (asm-set-labels! a l) (vector-set! a 2 l))
(define (asm-set-fixups! a f) (vector-set! a 3 f))
(define (asm-set-ctr! a n) (vector-set! a 4 n))

(define (emit-byte a b)
  (vector-set! (asm-buf a) (asm-pos a) (bitand b 255))
  (asm-set-pos! a (+ (asm-pos a) 1)))

(define (emit32 a i)
  (emit-byte a (bitand i 255))
  (emit-byte a (bitand (asr i 8) 255))
  (emit-byte a (bitand (asr i 16) 255))
  (emit-byte a (bitand (asr i 24) 255)))

(define (new-label a prefix)
  (let ((n (asm-ctr a)))
    (asm-set-ctr! a (+ n 1))
    (string-append prefix (number->string n))))

(define (def-label a name)
  (asm-set-labels! a (cons (cons name (asm-pos a)) (asm-labels a))))

(define (add-fixup a l t)
  (asm-set-fixups! a (cons (list (asm-pos a) l t) (asm-fixups a))))

(define (find-label ls name)
  (if (null? ls) 0
      (if (string=? (car (car ls)) name) (cdr (car ls)) (find-label (cdr ls) name))))

(define (lookup-label a name) (find-label (asm-labels a) name))

; ===== ARM64 Instructions =====

; Move immediate (handles full 64-bit values)
(define (e-movz a rd imm)
  (emit32 a (bitor MOVZ-BASE (bitor (lsl (bitand imm MASK-16BIT) 5) rd))))

(define (e-movz-hw a rd imm hw)
  (emit32 a (bitor MOVZ-BASE (bitor (lsl hw 21) (bitor (lsl (bitand imm MASK-16BIT) 5) rd)))))

(define (e-movk a rd imm hw)
  (emit32 a (bitor MOVK-BASE (bitor (lsl hw 21) (bitor (lsl (bitand imm MASK-16BIT) 5) rd)))))

(define (e-movn a rd imm)
  (emit32 a (bitor MOVN-BASE (bitor (lsl (bitand imm MASK-16BIT) 5) rd))))

; Load full 64-bit immediate
(define (e-mov-imm a rd imm)
  (if (and (>= imm 0) (< imm 65536))
      (e-movz a rd imm)
      (if (and (< imm 0) (> imm -65536))
          (e-movn a rd (bitand (bitnot imm) MASK-16BIT))
          (begin
            (e-movz a rd (bitand imm MASK-16BIT))
            (if (not (= 0 (bitand (asr imm 16) MASK-16BIT)))
                (e-movk a rd (bitand (asr imm 16) MASK-16BIT) 1) 0)
            (if (not (= 0 (bitand (asr imm 32) MASK-16BIT)))
                (e-movk a rd (bitand (asr imm 32) MASK-16BIT) 2) 0)
            (if (not (= 0 (bitand (asr imm 48) MASK-16BIT)))
                (e-movk a rd (bitand (asr imm 48) MASK-16BIT) 3) 0)))))

; Register moves
(define (e-mov a rd rm) (emit32 a (bitor ORR-BASE (bitor (lsl rm 16) rd))))

; Arithmetic
(define (e-add a rd rn rm) (emit32 a (bitor ADD-REG-BASE (bitor (lsl rm 16) (bitor (lsl rn 5) rd)))))
(define (e-sub a rd rn rm) (emit32 a (bitor SUB-REG-BASE (bitor (lsl rm 16) (bitor (lsl rn 5) rd)))))
(define (e-mul a rd rn rm) (emit32 a (bitor MUL-BASE (bitor (lsl rm 16) (bitor (lsl rn 5) rd)))))
(define (e-sdiv a rd rn rm) (emit32 a (bitor SDIV-BASE (bitor (lsl rm 16) (bitor (lsl rn 5) rd)))))
(define (e-udiv a rd rn rm) (emit32 a (bitor UDIV-BASE (bitor (lsl rm 16) (bitor (lsl rn 5) rd)))))
(define (e-msub a rd rn rm ra) (emit32 a (bitor MSUB-BASE (bitor (lsl rm 16) (bitor (lsl ra 10) (bitor (lsl rn 5) rd))))))
(define (e-neg a rd rm) (e-sub a rd XZR rm))

(define (e-add-i a rd rn imm) (emit32 a (bitor ADD-IMM-BASE (bitor (lsl (bitand imm MASK-12BIT) 10) (bitor (lsl rn 5) rd)))))
(define (e-sub-i a rd rn imm) (emit32 a (bitor SUB-IMM-BASE (bitor (lsl (bitand imm MASK-12BIT) 10) (bitor (lsl rn 5) rd)))))

; Bitwise
(define (e-and a rd rn rm) (emit32 a (bitor AND-BASE (bitor (lsl rm 16) (bitor (lsl rn 5) rd)))))
(define (e-orr a rd rn rm) (emit32 a (bitor 2852126720 (bitor (lsl rm 16) (bitor (lsl rn 5) rd))))) ; 0xAA000000
(define (e-eor a rd rn rm) (emit32 a (bitor EOR-BASE (bitor (lsl rm 16) (bitor (lsl rn 5) rd)))))
(define (e-lsl-reg a rd rn rm) (emit32 a (bitor LSL-REG-BASE (bitor (lsl rm 16) (bitor (lsl rn 5) rd)))))
(define (e-lsr-reg a rd rn rm) (emit32 a (bitor LSR-REG-BASE (bitor (lsl rm 16) (bitor (lsl rn 5) rd)))))
(define (e-asr-reg a rd rn rm) (emit32 a (bitor ASR-REG-BASE (bitor (lsl rm 16) (bitor (lsl rn 5) rd)))))

; Comparison
(define (e-cmp a rn rm) (emit32 a (bitor SUBS-REG-BASE (bitor (lsl rm 16) (lsl rn 5)))))
(define (e-cmp-i a rn imm) (emit32 a (bitor SUBS-IMM-BASE (bitor (lsl (bitand imm MASK-12BIT) 10) (lsl rn 5)))))
(define (e-cset a rd cond) (emit32 a (bitor CSINC-BASE (bitor (lsl (bitxor cond 1) 12) rd))))
(define (e-csel a rd rn rm cond) (emit32 a (bitor CSEL-BASE (bitor (lsl rm 16) (bitor (lsl cond 12) (bitor (lsl rn 5) rd))))))

; Memory
(define (e-ldr a rt rn imm)
  (let ((sc (asr imm 3)))
    (emit32 a (bitor LDR-IMM-BASE (bitor (lsl (bitand sc MASK-12BIT) 10) (bitor (lsl rn 5) rt))))))
(define (e-str a rt rn imm)
  (let ((sc (asr imm 3)))
    (emit32 a (bitor STR-IMM-BASE (bitor (lsl (bitand sc MASK-12BIT) 10) (bitor (lsl rn 5) rt))))))
(define (e-ldrb a rt rn imm)
  (emit32 a (bitor LDRB-IMM-BASE (bitor (lsl (bitand imm MASK-12BIT) 10) (bitor (lsl rn 5) rt)))))
(define (e-strb a rt rn imm)
  (emit32 a (bitor 939524096 (bitor (lsl (bitand imm MASK-12BIT) 10) (bitor (lsl rn 5) rt)))))
(define (e-ldr-post a rt rn imm)
  (emit32 a (bitor LDR-POST-BASE (bitor (lsl (bitand imm MASK-9BIT) 12) (bitor (lsl rn 5) rt)))))
(define (e-str-pre a rt rn imm)
  (emit32 a (bitor STR-PRE-BASE (bitor (lsl (bitand imm MASK-9BIT) 12) (bitor (lsl rn 5) rt)))))
(define (e-stp-pre a r1 r2 rn imm)
  (let ((sc (bitand (asr imm 3) MASK-7BIT)))
    (emit32 a (bitor STP-PRE-BASE (bitor (lsl sc 15) (bitor (lsl r2 10) (bitor (lsl rn 5) r1)))))))
(define (e-ldp-post a r1 r2 rn imm)
  (let ((sc (bitand (asr imm 3) MASK-7BIT)))
    (emit32 a (bitor LDP-POST-BASE (bitor (lsl sc 15) (bitor (lsl r2 10) (bitor (lsl rn 5) r1)))))))

; Branches
(define (e-b a l) (add-fixup a l 0) (emit32 a B-BASE))
(define (e-bl a l) (add-fixup a l 1) (emit32 a BL-BASE))
(define (e-blr a rn) (emit32 a (bitor BLR-BASE (lsl rn 5))))
(define (e-br a rn) (emit32 a (bitor BR-BASE (lsl rn 5))))
(define (e-ret a) (emit32 a RET-INSTR))
(define (e-bcond a cond l) (add-fixup a l 2) (emit32 a (bitor BCOND-BASE cond)))
(define (e-nop a) (emit32 a NOP-INSTR))

; System
(define (e-svc a imm) (emit32 a (bitor SVC-BASE (lsl imm 5))))

; Convenience
(define (e-prologue a) (e-stp-pre a X29 X30 SP -16) (e-mov a X29 SP))
(define (e-epilogue a) (e-ldp-post a X29 X30 SP 16) (e-ret a))
(define (e-push-x0 a) (e-str-pre a X0 SP -16))
(define (e-pop-x0 a) (e-ldr-post a X0 SP 16))
(define (e-pop-x1 a) (e-ldr-post a X1 SP 16))
(define (e-alloc a sz) (e-sub-i a SP SP sz))
(define (e-free a sz) (e-add-i a SP SP sz))

; ===== Syscall Helpers =====

; exit(code) - X0 = exit code
(define (e-sys-exit a)
  (e-mov-imm a X16 SYS-EXIT)
  (e-svc a 128))

; write(fd, buf, len) - X0=fd, X1=buf, X2=len, returns bytes written in X0
(define (e-sys-write a)
  (e-mov-imm a X16 SYS-WRITE)
  (e-svc a 128))

; read(fd, buf, len) - X0=fd, X1=buf, X2=len, returns bytes read in X0
(define (e-sys-read a)
  (e-mov-imm a X16 SYS-READ)
  (e-svc a 128))

; open(path, flags, mode) - X0=path, X1=flags, X2=mode, returns fd in X0
(define (e-sys-open a)
  (e-mov-imm a X16 SYS-OPEN)
  (e-svc a 128))

; close(fd) - X0=fd
(define (e-sys-close a)
  (e-mov-imm a X16 SYS-CLOSE)
  (e-svc a 128))

; mmap(addr, len, prot, flags, fd, offset)
; X0=addr, X1=len, X2=prot, X3=flags, X4=fd, X5=offset
(define (e-sys-mmap a)
  (e-mov-imm a X16 SYS-MMAP)
  (e-svc a 128))

; ===== Fixup Resolution =====
(define (resolve-fixups a) (resolve-fx-h a (asm-fixups a)))

(define (resolve-fx-h a fx)
  (if (null? fx) 0
      (begin (resolve-one a (car fx)) (resolve-fx-h a (cdr fx)))))

(define (resolve-one a fx)
  (let ((off (car fx)) (lbl (car (cdr fx))) (typ (car (cdr (cdr fx)))))
    (let ((tgt (lookup-label a lbl)) (buf (asm-buf a)))
      (let ((ioff (asr (- tgt off) 2)))
        (let ((inst (bitor (vector-ref buf off)
                          (bitor (lsl (vector-ref buf (+ off 1)) 8)
                                (bitor (lsl (vector-ref buf (+ off 2)) 16)
                                      (lsl (vector-ref buf (+ off 3)) 24))))))
          (let ((patched (if (= typ 2)
                            (bitor inst (lsl (bitand ioff MASK-19BIT) 5))
                            (bitor inst (bitand ioff MASK-26BIT)))))
            (vector-set! buf off (bitand patched 255))
            (vector-set! buf (+ off 1) (bitand (asr patched 8) 255))
            (vector-set! buf (+ off 2) (bitand (asr patched 16) 255))
            (vector-set! buf (+ off 3) (bitand (asr patched 24) 255))))))))

; ===== Lexer =====
(define (make-lexer source) (cons source 0))
(define (lexer-eof? lex) (>= (cdr lex) (string-length (car lex))))
(define (lexer-peek lex) (if (lexer-eof? lex) 0 (string-ref (car lex) (cdr lex))))
(define (lexer-advance lex) (cons (car lex) (+ (cdr lex) 1)))
(define (make-token type value) (cons type value))

(define (skip-whitespace lex)
  (if (lexer-eof? lex) lex
      (if (char-whitespace? (lexer-peek lex))
          (skip-whitespace (lexer-advance lex))
          lex)))

(define (skip-line-comment lex)
  (if (lexer-eof? lex) lex
      (let ((ch (lexer-peek lex)))
        (if (= ch 10) (lexer-advance lex)
            (skip-line-comment (lexer-advance lex))))))

(define (skip-ws-comments lex)
  (let ((lex2 (skip-whitespace lex)))
    (if (lexer-eof? lex2) lex2
        (if (= (lexer-peek lex2) 59)
            (skip-ws-comments (skip-line-comment (lexer-advance lex2)))
            lex2))))

(define (symbol-char? c)
  (or (char-alphabetic? c) (char-numeric? c)
      (= c 45) (= c 43) (= c 42) (= c 47) (= c 60) (= c 62) (= c 61)
      (= c 95) (= c 33) (= c 63) (= c 38) (= c 124)))

(define (read-number-h lex acc)
  (if (lexer-eof? lex) (cons acc lex)
      (let ((ch (lexer-peek lex)))
        (if (char-numeric? ch)
            (read-number-h (lexer-advance lex) (+ (* acc 10) (- ch 48)))
            (cons acc lex)))))

(define (read-number lex) (read-number-h lex 0))

(define (chars-to-string-h chars acc)
  (if (null? chars) acc
      (chars-to-string-h (cdr chars) (string-append acc (make-char-string (car chars))))))

(define (chars-to-string chars) (chars-to-string-h chars ""))

(define (read-symbol-h lex chars)
  (if (lexer-eof? lex) (cons chars lex)
      (let ((ch (lexer-peek lex)))
        (if (symbol-char? ch)
            (read-symbol-h (lexer-advance lex) (cons ch chars))
            (cons chars lex)))))

(define (read-symbol lex)
  (let ((result (read-symbol-h lex (list))))
    (cons (chars-to-string (reverse (car result))) (cdr result))))

(define (read-string-h lex chars)
  (if (lexer-eof? lex) (cons chars lex)
      (let ((ch (lexer-peek lex)))
        (if (= ch 34) (cons chars (lexer-advance lex))
            (if (= ch 92)
                (let ((lex2 (lexer-advance lex)))
                  (if (lexer-eof? lex2) (cons chars lex2)
                      (let ((esc (lexer-peek lex2)))
                        (read-string-h (lexer-advance lex2)
                                      (cons (if (= esc 110) 10 (if (= esc 116) 9 esc)) chars)))))
                (read-string-h (lexer-advance lex) (cons ch chars)))))))

(define (read-string lex)
  (let ((result (read-string-h lex (list))))
    (cons (chars-to-string (reverse (car result))) (cdr result))))

(define (read-token lex)
  (let ((lex2 (skip-ws-comments lex)))
    (if (lexer-eof? lex2) (list)
        (let ((ch (lexer-peek lex2)))
          (if (= ch 40) (cons (make-token 0 "(") (lexer-advance lex2))
              (if (= ch 41) (cons (make-token 1 ")") (lexer-advance lex2))
                  (if (= ch 39) (cons (make-token 5 "'") (lexer-advance lex2))
                      (if (= ch 34)
                          (let ((r (read-string (lexer-advance lex2))))
                            (cons (make-token 4 (car r)) (cdr r)))
                          (if (char-numeric? ch)
                              (let ((r (read-number lex2)))
                                (cons (make-token 2 (car r)) (cdr r)))
                              (if (= ch 45)
                                  (let ((lex3 (lexer-advance lex2)))
                                    (if (and (not (lexer-eof? lex3)) (char-numeric? (lexer-peek lex3)))
                                        (let ((r (read-number lex3)))
                                          (cons (make-token 2 (- 0 (car r))) (cdr r)))
                                        (let ((r (read-symbol lex2)))
                                          (cons (make-token 3 (car r)) (cdr r)))))
                                  (let ((r (read-symbol lex2)))
                                    (cons (make-token 3 (car r)) (cdr r)))))))))))))

(define (tokenize-h lex tokens)
  (let ((result (read-token lex)))
    (if (null? result) (reverse tokens)
        (tokenize-h (cdr result) (cons (car result) tokens)))))

(define (tokenize source) (tokenize-h (make-lexer source) (list)))

; ===== Parser =====
(define (token-type tok) (car tok))
(define (token-value tok) (cdr tok))
(define (make-parser tokens) (cons tokens 0))
(define (parser-tokens p) (car p))
(define (parser-pos p) (cdr p))
(define (parser-set-pos p pos) (cons (car p) pos))
(define (parser-eof? p) (>= (parser-pos p) (list-length (parser-tokens p))))
(define (parser-current p) (if (parser-eof? p) (list) (list-ref (parser-tokens p) (parser-pos p))))
(define (parser-advance p) (parser-set-pos p (+ (parser-pos p) 1)))

(define (parse-expr p)
  (if (parser-eof? p) (list)
      (let ((tok (parser-current p)))
        (let ((typ (token-type tok)) (val (token-value tok)))
          (if (= typ 0) (parse-list (parser-advance p))
              (if (= typ 1) (list)
                  (if (= typ 2) (cons val (parser-advance p))
                      (if (= typ 3) (cons (cons 3 val) (parser-advance p))
                          (if (= typ 4) (cons (cons 4 val) (parser-advance p))
                              (if (= typ 5)
                                  (let ((quoted (parse-expr (parser-advance p))))
                                    (if (null? quoted) (list)
                                        (cons (list (cons 3 "quote") (car quoted)) (cdr quoted))))
                                  (list)))))))))))

(define (parse-list-h p elements)
  (if (parser-eof? p) (list)
      (let ((tok (parser-current p)))
        (if (= (token-type tok) 1)
            (cons (reverse elements) (parser-advance p))
            (let ((result (parse-expr p)))
              (if (null? result) (list)
                  (parse-list-h (cdr result) (cons (car result) elements))))))))

(define (parse-list p) (parse-list-h p (list)))

(define (parse-all-h p exprs)
  (if (parser-eof? p) (reverse exprs)
      (let ((result (parse-expr p)))
        (if (null? result) (reverse exprs)
            (parse-all-h (cdr result) (cons (car result) exprs))))))

(define (parse tokens) (parse-all-h (make-parser tokens) (list)))

; ===== Code Generator =====
(define (make-cg)
  (let ((cg (make-vector 8)))
    (vector-set! cg 0 (make-asm))    ; assembler
    (vector-set! cg 1 (list))         ; functions
    (vector-set! cg 2 (list))         ; globals
    (vector-set! cg 3 (list))         ; locals
    (vector-set! cg 4 0)              ; temp depth
    (vector-set! cg 5 0)              ; local stack size
    (vector-set! cg 6 (list))         ; string constants
    (vector-set! cg 7 0)              ; string data offset
    cg))

(define (cg-asm cg) (vector-ref cg 0))
(define (cg-funcs cg) (vector-ref cg 1))
(define (cg-globals cg) (vector-ref cg 2))
(define (cg-locals cg) (vector-ref cg 3))
(define (cg-depth cg) (vector-ref cg 4))
(define (cg-lsize cg) (vector-ref cg 5))
(define (cg-strings cg) (vector-ref cg 6))
(define (cg-stroff cg) (vector-ref cg 7))
(define (cg-set-funcs! cg f) (vector-set! cg 1 f))
(define (cg-set-globals! cg g) (vector-set! cg 2 g))
(define (cg-set-locals! cg l) (vector-set! cg 3 l))
(define (cg-set-depth! cg d) (vector-set! cg 4 d))
(define (cg-set-lsize! cg s) (vector-set! cg 5 s))
(define (cg-set-strings! cg s) (vector-set! cg 6 s))
(define (cg-set-stroff! cg o) (vector-set! cg 7 o))

; AST helpers
(define (ast-number? n) (not (pair? n)))
(define (ast-symbol? n) (and (pair? n) (= (car n) 3)))
(define (ast-string? n) (and (pair? n) (= (car n) 4)))
(define (ast-list? n) (and (pair? n) (not (= (car n) 3)) (not (= (car n) 4))))
(define (ast-sym-name n) (cdr n))
(define (ast-str-val n) (cdr n))

; Helpers
(define (push-temp cg) (e-push-x0 (cg-asm cg)) (cg-set-depth! cg (+ (cg-depth cg) 16)))
(define (pop-to-x0 cg) (e-pop-x0 (cg-asm cg)) (cg-set-depth! cg (- (cg-depth cg) 16)))
(define (pop-to-x1 cg) (e-pop-x1 (cg-asm cg)) (cg-set-depth! cg (- (cg-depth cg) 16)))

(define (lookup-alist key alist)
  (if (null? alist) (list)
      (if (string=? key (car (car alist))) (cdr (car alist))
          (lookup-alist key (cdr alist)))))

(define (lookup-func cg name) (lookup-alist name (cg-funcs cg)))
(define (lookup-global cg name) (lookup-alist name (cg-globals cg)))
(define (lookup-local cg name) (lookup-alist name (cg-locals cg)))

; Generate expression
(define (gen-expr cg node)
  (if (ast-number? node) (e-mov-imm (cg-asm cg) X0 node)
      (if (ast-symbol? node) (gen-sym cg node)
          (if (ast-string? node) (gen-string cg node)
              (if (ast-list? node) (gen-list cg node)
                  (e-mov-imm (cg-asm cg) X0 0))))))

(define (gen-sym cg node)
  (let ((name (ast-sym-name node)))
    (let ((local (lookup-local cg name)))
      (if (not (null? local))
          (e-ldr (cg-asm cg) X0 SP (+ local (cg-depth cg)))
          (let ((global (lookup-global cg name)))
            (if (not (null? global))
                (begin
                  (e-mov-imm (cg-asm cg) X0 global)
                  (e-ldr (cg-asm cg) X0 X0 0))
                (e-mov-imm (cg-asm cg) X0 0)))))))

(define (gen-string cg node)
  ; For now, strings become addresses to data section
  (let ((str (ast-str-val node))
        (lbl (new-label (cg-asm cg) "_str")))
    (cg-set-strings! cg (cons (cons lbl str) (cg-strings cg)))
    (e-bl (cg-asm cg) lbl)))  ; Will be fixed up

(define (gen-list cg node)
  (if (null? node) (e-mov-imm (cg-asm cg) X0 0)
      (let ((first (car node)))
        (if (ast-symbol? first)
            (let ((op (ast-sym-name first)) (args (cdr node)))
              (gen-op cg op args))
            (e-mov-imm (cg-asm cg) X0 0)))))

(define (gen-op cg op args)
  (if (string=? op "+") (gen-arith cg args 0)
      (if (string=? op "-") (gen-arith cg args 1)
          (if (string=? op "*") (gen-arith cg args 2)
              (if (string=? op "/") (gen-arith cg args 3)
                  (if (string=? op "mod") (gen-mod cg args)
                      (if (string=? op "=") (gen-cmp cg args COND-EQ)
                          (if (string=? op "<") (gen-cmp cg args COND-LT)
                              (if (string=? op ">") (gen-cmp cg args COND-GT)
                                  (if (string=? op "<=") (gen-cmp cg args COND-LE)
                                      (if (string=? op ">=") (gen-cmp cg args COND-GE)
                                          (if (string=? op "bitand") (gen-bitop cg args 0)
                                              (if (string=? op "bitor") (gen-bitop cg args 1)
                                                  (if (string=? op "bitxor") (gen-bitop cg args 2)
                                                      (if (string=? op "bitnot") (gen-bitnot cg args)
                                                          (if (string=? op "lsl") (gen-shift cg args 0)
                                                              (if (string=? op "lsr") (gen-shift cg args 1)
                                                                  (if (string=? op "asr") (gen-shift cg args 2)
                                                                      (if (string=? op "if") (gen-if cg args)
                                                                          (if (string=? op "let") (gen-let cg args)
                                                                              (if (string=? op "begin") (gen-begin cg args)
                                                                                  (if (string=? op "and") (gen-and cg args)
                                                                                      (if (string=? op "or") (gen-or cg args)
                                                                                          (if (string=? op "not") (gen-not cg args)
                                                                                              (if (string=? op "define") 0
                                                                                                  (if (string=? op "write") (gen-write cg args)
                                                                                                      (if (string=? op "exit") (gen-exit cg args)
                                                                                                          (gen-call cg op args))))))))))))))))))))))))))))

(define (gen-arith cg args operation)
  (if (null? args) (e-mov-imm (cg-asm cg) X0 0)
      (if (null? (cdr args))
          (begin
            (gen-expr cg (car args))
            (if (= operation 1) (e-neg (cg-asm cg) X0 X0) 0))
          (begin
            (gen-expr cg (car args))
            (push-temp cg)
            (gen-arith-rest cg (cdr args) operation)))))

(define (gen-arith-rest cg args operation)
  (gen-expr cg (car args))
  (pop-to-x1 cg)
  (let ((a (cg-asm cg)))
    (if (= operation 0) (e-add a X0 X1 X0)
        (if (= operation 1) (e-sub a X0 X1 X0)
            (if (= operation 2) (e-mul a X0 X1 X0)
                (e-sdiv a X0 X1 X0)))))
  (if (not (null? (cdr args)))
      (begin (push-temp cg) (gen-arith-rest cg (cdr args) operation))
      0))

(define (gen-mod cg args)
  (gen-expr cg (car args))
  (push-temp cg)
  (gen-expr cg (car (cdr args)))
  (pop-to-x1 cg)
  (let ((a (cg-asm cg)))
    (e-sdiv a X2 X1 X0)      ; X2 = X1 / X0
    (e-msub a X0 X2 X0 X1))) ; X0 = X1 - X2*X0

(define (gen-bitop cg args op)
  (gen-expr cg (car args))
  (push-temp cg)
  (gen-expr cg (car (cdr args)))
  (pop-to-x1 cg)
  (let ((a (cg-asm cg)))
    (if (= op 0) (e-and a X0 X1 X0)
        (if (= op 1) (e-orr a X0 X1 X0)
            (e-eor a X0 X1 X0)))))

(define (gen-bitnot cg args)
  (gen-expr cg (car args))
  (e-mov-imm (cg-asm cg) X1 -1)
  (e-eor (cg-asm cg) X0 X0 X1))

(define (gen-shift cg args op)
  (gen-expr cg (car args))
  (push-temp cg)
  (gen-expr cg (car (cdr args)))
  (pop-to-x1 cg)
  (let ((a (cg-asm cg)))
    (if (= op 0) (e-lsl-reg a X0 X1 X0)
        (if (= op 1) (e-lsr-reg a X0 X1 X0)
            (e-asr-reg a X0 X1 X0)))))

(define (gen-cmp cg args cond-code)
  (gen-expr cg (car args))
  (push-temp cg)
  (gen-expr cg (car (cdr args)))
  (pop-to-x1 cg)
  (e-cmp (cg-asm cg) X1 X0)
  (e-cset (cg-asm cg) X0 cond-code))

(define (gen-if cg args)
  (let ((a (cg-asm cg))
        (else-lbl (new-label (cg-asm cg) "else"))
        (end-lbl (new-label (cg-asm cg) "end")))
    (gen-expr cg (car args))
    (e-cmp-i a X0 0)
    (e-bcond a COND-EQ else-lbl)
    (gen-expr cg (car (cdr args)))
    (e-b a end-lbl)
    (def-label a else-lbl)
    (if (not (null? (cdr (cdr args))))
        (gen-expr cg (car (cdr (cdr args))))
        (e-mov-imm a X0 0))
    (def-label a end-lbl)))

(define (gen-and cg args)
  (let ((a (cg-asm cg))
        (false-lbl (new-label a "false"))
        (end-lbl (new-label a "end")))
    (gen-and-h cg args false-lbl)
    (e-mov-imm a X0 1)
    (e-b a end-lbl)
    (def-label a false-lbl)
    (e-mov-imm a X0 0)
    (def-label a end-lbl)))

(define (gen-and-h cg args false-lbl)
  (if (null? args) 0
      (begin
        (gen-expr cg (car args))
        (e-cmp-i (cg-asm cg) X0 0)
        (e-bcond (cg-asm cg) COND-EQ false-lbl)
        (gen-and-h cg (cdr args) false-lbl))))

(define (gen-or cg args)
  (let ((a (cg-asm cg))
        (true-lbl (new-label a "true"))
        (end-lbl (new-label a "end")))
    (gen-or-h cg args true-lbl)
    (e-mov-imm a X0 0)
    (e-b a end-lbl)
    (def-label a true-lbl)
    (e-mov-imm a X0 1)
    (def-label a end-lbl)))

(define (gen-or-h cg args true-lbl)
  (if (null? args) 0
      (begin
        (gen-expr cg (car args))
        (e-cmp-i (cg-asm cg) X0 0)
        (e-bcond (cg-asm cg) COND-NE true-lbl)
        (gen-or-h cg (cdr args) true-lbl))))

(define (gen-not cg args)
  (gen-expr cg (car args))
  (e-cmp-i (cg-asm cg) X0 0)
  (e-cset (cg-asm cg) X0 COND-EQ))

(define (gen-let cg args)
  (let ((bindings (car args)) (body (cdr args)))
    (let ((num (list-length bindings))
          (saved-locals (cg-locals cg))
          (saved-depth (cg-depth cg)))
      (if (> num 0)
          (let ((stack-sz (* (+ (/ num 2) 1) 16)))
            (e-alloc (cg-asm cg) stack-sz)
            (cg-set-depth! cg (+ (cg-depth cg) stack-sz))
            (gen-let-bindings cg bindings 0 stack-sz)
            (gen-begin cg body)
            (e-free (cg-asm cg) stack-sz)
            (cg-set-depth! cg saved-depth))
          (gen-begin cg body))
      (cg-set-locals! cg saved-locals))))

(define (gen-let-bindings cg bindings idx stack-sz)
  (if (null? bindings) 0
      (let ((binding (car bindings)))
        (let ((var (ast-sym-name (car binding))) (val (car (cdr binding))))
          (gen-expr cg val)
          (let ((sp-off (- stack-sz (* (+ idx 1) 8))))
            (e-str (cg-asm cg) X0 SP sp-off)
            (cg-set-locals! cg (cons (cons var (- sp-off (cg-depth cg))) (cg-locals cg))))
          (gen-let-bindings cg (cdr bindings) (+ idx 1) stack-sz)))))

(define (gen-begin cg args)
  (if (null? args) 0
      (begin (gen-expr cg (car args)) (gen-begin cg (cdr args)))))

(define (gen-write cg args)
  ; (write fd buf len)
  (let ((a (cg-asm cg)))
    (gen-expr cg (car args))       ; fd
    (push-temp cg)
    (gen-expr cg (car (cdr args))) ; buf
    (push-temp cg)
    (gen-expr cg (car (cdr (cdr args)))) ; len
    (e-mov a X2 X0)                ; len -> X2
    (pop-to-x1 cg)                 ; buf -> X1
    (pop-to-x0 cg)                 ; fd -> X0
    (e-sys-write a)))

(define (gen-exit cg args)
  (gen-expr cg (car args))
  (e-sys-exit (cg-asm cg)))

(define (gen-call cg name args)
  (let ((func (lookup-func cg name)))
    (if (null? func) (e-mov-imm (cg-asm cg) X0 0)
        (let ((num (list-length args)))
          (gen-push-args cg args)
          (gen-pop-args cg num 0)
          (e-bl (cg-asm cg) (string-append "_lisp_" name))))))

(define (gen-push-args cg args)
  (if (null? args) 0
      (begin (gen-push-args cg (cdr args)) (gen-expr cg (car args)) (push-temp cg))))

(define (gen-pop-args cg num idx)
  (if (>= idx num) 0
      (begin
        (e-ldr-post (cg-asm cg) idx SP 16)
        (cg-set-depth! cg (- (cg-depth cg) 16))
        (gen-pop-args cg num (+ idx 1)))))

; Generate function
(define (gen-func cg name params body)
  (let ((a (cg-asm cg)))
    (def-label a (string-append "_lisp_" name))
    (e-prologue a)
    (cg-set-locals! cg (list))
    (cg-set-depth! cg 0)
    (let ((num-params (list-length params)))
      (if (> num-params 0)
          (let ((stack-sz (* (+ (/ num-params 2) 1) 16)))
            (e-alloc a stack-sz)
            (cg-set-lsize! cg stack-sz)
            (store-params cg params 0 stack-sz)
            (gen-begin cg body)
            (e-free a stack-sz))
          (gen-begin cg body)))
    (e-epilogue a)
    (cg-set-locals! cg (list))
    (cg-set-depth! cg 0)
    (cg-set-lsize! cg 0)))

(define (store-params cg params idx stack-sz)
  (if (null? params) 0
      (let ((param-name (if (ast-symbol? (car params)) (ast-sym-name (car params)) "?"))
            (sp-off (- stack-sz (* (+ idx 1) 8))))
        (e-str (cg-asm cg) idx SP sp-off)
        (cg-set-locals! cg (cons (cons param-name sp-off) (cg-locals cg)))
        (store-params cg (cdr params) (+ idx 1) stack-sz))))

; Collect definitions
(define (collect-funcs ast funcs)
  (if (null? ast) funcs
      (let ((node (car ast)))
        (if (ast-list? node)
            (if (and (not (null? node)) (ast-symbol? (car node)) (string=? (ast-sym-name (car node)) "define"))
                (if (> (list-length node) 2)
                    (let ((name-or-sig (list-ref node 1)))
                      (if (ast-list? name-or-sig)
                          (let ((fname (ast-sym-name (car name-or-sig)))
                                (params (cdr name-or-sig))
                                (body (cdr (cdr node))))
                            (collect-funcs (cdr ast) (cons (list fname params body) funcs)))
                          (collect-funcs (cdr ast) funcs)))
                    (collect-funcs (cdr ast) funcs))
                (collect-funcs (cdr ast) funcs))
            (collect-funcs (cdr ast) funcs)))))

(define (make-func-alist funcs)
  (if (null? funcs) (list)
      (let ((f (car funcs)))
        (cons (cons (car f) (cdr f)) (make-func-alist (cdr funcs))))))

; Compile program
(define (compile-prog cg ast)
  (let ((funcs (collect-funcs ast (list))))
    (cg-set-funcs! cg (make-func-alist funcs))
    (let ((a (cg-asm cg)))
      ; Jump to main
      (e-b a "_main")
      ; Generate all functions
      (gen-all-funcs cg funcs)
      ; Generate main
      (def-label a "_main")
      (e-prologue a)
      (gen-toplevel cg ast)
      ; Exit with return value
      (e-sys-exit a)
      ; (e-epilogue a)  ; Not needed - we exit
      (resolve-fixups a))))

(define (gen-all-funcs cg funcs)
  (if (null? funcs) 0
      (begin
        (let ((f (car funcs)))
          (gen-func cg (car f) (car (cdr f)) (car (cdr (cdr f)))))
        (gen-all-funcs cg (cdr funcs)))))

(define (gen-toplevel cg ast)
  (if (null? ast) 0
      (let ((node (car ast)))
        (if (ast-list? node)
            (if (and (not (null? node)) (ast-symbol? (car node)) (string=? (ast-sym-name (car node)) "define"))
                (gen-toplevel cg (cdr ast))
                (begin (gen-expr cg node) (gen-toplevel cg (cdr ast))))
            (begin (gen-expr cg node) (gen-toplevel cg (cdr ast)))))))

; ===== Output =====
(define (output-bytes cg)
  (let ((a (cg-asm cg))
        (buf (asm-buf (cg-asm cg)))
        (len (asm-pos (cg-asm cg))))
    (print len)
    (output-bytes-h buf 0 len)))

(define (output-bytes-h buf idx len)
  (if (>= idx len) 0
      (begin (print (vector-ref buf idx)) (output-bytes-h buf (+ idx 1) len))))

; ===== Test =====
; Compile: factorial(5) that exits with result as exit code
(define source "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) (fact 5)")

(define tokens (tokenize source))
(define ast (parse tokens))
(define cg (make-cg))
(compile-prog cg ast)
(output-bytes cg)
