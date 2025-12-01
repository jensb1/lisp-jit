; Self-Hosting Lisp Compiler
; Compiles Lisp source to standalone ARM64 macOS executables
; After bootstrap, requires no TypeScript runtime

; ===== Register Constants =====
(define X0 0) (define X1 1) (define X2 2) (define X3 3)
(define X4 4) (define X5 5) (define X8 8) (define X9 9)
(define X10 10) (define X11 11) (define X12 12) (define X13 13)
(define X14 14) (define X15 15) (define X16 16)
(define X19 19) (define X20 20) (define X21 21) (define X22 22)
(define X28 28) (define X29 29) (define X30 30)
(define SP 31) (define XZR 31)

(define COND-EQ 0) (define COND-NE 1)
(define COND-GE 10) (define COND-LT 11)
(define COND-GT 12) (define COND-LE 13)

; macOS syscalls
(define SYS-EXIT 1)
(define SYS-READ 3)
(define SYS-WRITE 4)
(define SYS-OPEN 5)
(define SYS-CLOSE 6)

; ARM64 instruction bases
(define MOVZ-BASE 3531603968)
(define MOVN-BASE 2457862144)
(define MOVK-BASE 4068474880)
(define ORR-BASE 2852127712)
(define AND-BASE 2315255808)
(define EOR-BASE 3388997632)
(define ADD-REG-BASE 2332033024)
(define ADD-IMM-BASE 2432696320)
(define SUB-REG-BASE 3405774848)
(define SUB-IMM-BASE 3506438144)
(define MUL-BASE 2600500224)
(define SDIV-BASE 2596277248)
(define UDIV-BASE 2596276224)
(define MSUB-BASE 2600501248)
(define LSL-REG-BASE 2596274176)
(define LSR-REG-BASE 2596274432)
(define ASR-REG-BASE 2596274688)
(define SUBS-REG-BASE 3942645791)  ; 0xEB00001F - includes XZR in rd field
(define SUBS-IMM-BASE 4043309087)  ; 0xF100001F - includes XZR in rd field
(define CSINC-BASE 2594113504)  ; 0x9A9F07E0 - with XZR in Rm and Rn
(define LDR-IMM-BASE 4181721088)
(define STR-IMM-BASE 4177526784)
(define LDRB-IMM-BASE 960495616)  ; 0x39400000
(define STRB-IMM-BASE 939524096)
(define LDR-POST-BASE 4164944896)
(define STR-PRE-BASE 4160752640)
(define STP-PRE-BASE 2843738112)
(define LDP-POST-BASE 2831155200)
(define B-BASE 335544320)
(define BL-BASE 2483027968)
(define BR-BASE 3594387456)
(define BLR-BASE 3594452992)
(define RET-INSTR 3596551104)
(define BCOND-BASE 1409286144)
(define SVC-BASE 3556769793)  ; 0xD4000001 - correct SVC encoding
(define NOP-INSTR 3573751839)
(define ADR-BASE 268435456)

; Masks
(define MASK-16BIT 65535)
(define MASK-12BIT 4095)
(define MASK-9BIT 511)
(define MASK-7BIT 127)
(define MASK-26BIT 67108863)
(define MASK-19BIT 524287)
(define MASK-21BIT 2097151)

; Mach-O object file constants
(define MH-MAGIC-64 4277009103)        ; 0xFEEDFACF
(define CPU-TYPE-ARM64 16777228)       ; 0x0100000C
(define MH-OBJECT 1)                   ; Object file type
(define MH-SUBSECTIONS-VIA-SYMBOLS 8192)  ; 0x2000
(define LC-SEGMENT-64 25)              ; 0x19
(define LC-SYMTAB 2)
(define LC-BUILD-VERSION 50)           ; 0x32
(define VM-PROT-READ 1)
(define VM-PROT-EXECUTE 4)
(define S-ATTR-PURE-INSTRUCTIONS 2147483648)  ; 0x80000000
(define S-ATTR-SOME-INSTRUCTIONS 1024)        ; 0x400
(define PLATFORM-MACOS 1)
(define N-EXT 1)                       ; External symbol
(define N-SECT 14)                     ; 0x0e - symbol in section

; ===== Utilities =====
(define (list-length lst) (if (null? lst) 0 (+ 1 (list-length (cdr lst)))))
(define (list-ref lst n) (if (= n 0) (car lst) (list-ref (cdr lst) (- n 1))))
(define (reverse-h lst acc) (if (null? lst) acc (reverse-h (cdr lst) (cons (car lst) acc))))
(define (reverse lst) (reverse-h lst (list)))
(define (mod a b) (- a (* (/ a b) b)))

; ===== Assembler =====
(define (make-asm)
  (let ((s (make-vector 6)))
    (vector-set! s 0 (make-vector 8192))
    (vector-set! s 1 0)
    (vector-set! s 2 (list))
    (vector-set! s 3 (list))
    (vector-set! s 4 0)
    (vector-set! s 5 (list))
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
(define (asm-set-data! a d) (vector-set! a 5 d))

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

; Instructions
(define (e-movz a rd imm) (emit32 a (bitor MOVZ-BASE (bitor (lsl (bitand imm MASK-16BIT) 5) rd))))
(define (e-movk a rd imm hw) (emit32 a (bitor MOVK-BASE (bitor (lsl hw 21) (bitor (lsl (bitand imm MASK-16BIT) 5) rd)))))
(define (e-movn a rd imm) (emit32 a (bitor MOVN-BASE (bitor (lsl (bitand imm MASK-16BIT) 5) rd))))

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

(define (e-mov a rd rm) (emit32 a (bitor ORR-BASE (bitor (lsl rm 16) rd))))
(define (e-add a rd rn rm) (emit32 a (bitor ADD-REG-BASE (bitor (lsl rm 16) (bitor (lsl rn 5) rd)))))
(define (e-sub a rd rn rm) (emit32 a (bitor SUB-REG-BASE (bitor (lsl rm 16) (bitor (lsl rn 5) rd)))))
(define (e-mul a rd rn rm) (emit32 a (bitor MUL-BASE (bitor (lsl rm 16) (bitor (lsl rn 5) rd)))))
(define (e-sdiv a rd rn rm) (emit32 a (bitor SDIV-BASE (bitor (lsl rm 16) (bitor (lsl rn 5) rd)))))
(define (e-udiv a rd rn rm) (emit32 a (bitor UDIV-BASE (bitor (lsl rm 16) (bitor (lsl rn 5) rd)))))
(define (e-msub a rd rn rm ra) (emit32 a (bitor MSUB-BASE (bitor (lsl rm 16) (bitor (lsl ra 10) (bitor (lsl rn 5) rd))))))
(define (e-neg a rd rm) (e-sub a rd XZR rm))
(define (e-add-i a rd rn imm) (emit32 a (bitor ADD-IMM-BASE (bitor (lsl (bitand imm MASK-12BIT) 10) (bitor (lsl rn 5) rd)))))
(define (e-sub-i a rd rn imm) (emit32 a (bitor SUB-IMM-BASE (bitor (lsl (bitand imm MASK-12BIT) 10) (bitor (lsl rn 5) rd)))))
(define (e-and a rd rn rm) (emit32 a (bitor AND-BASE (bitor (lsl rm 16) (bitor (lsl rn 5) rd)))))
(define (e-orr a rd rn rm) (emit32 a (bitor 2852126720 (bitor (lsl rm 16) (bitor (lsl rn 5) rd)))))
(define (e-eor a rd rn rm) (emit32 a (bitor EOR-BASE (bitor (lsl rm 16) (bitor (lsl rn 5) rd)))))
(define (e-lsl-reg a rd rn rm) (emit32 a (bitor LSL-REG-BASE (bitor (lsl rm 16) (bitor (lsl rn 5) rd)))))
(define (e-lsr-reg a rd rn rm) (emit32 a (bitor LSR-REG-BASE (bitor (lsl rm 16) (bitor (lsl rn 5) rd)))))
(define (e-asr-reg a rd rn rm) (emit32 a (bitor ASR-REG-BASE (bitor (lsl rm 16) (bitor (lsl rn 5) rd)))))
; LSL immediate: UBFM Rd, Rn, #(64-shift), #(63-shift)
(define UBFM-BASE 3543359488)  ; 0xD3400000
(define (e-lsl-i a rd rn shift)
  (let ((immr (bitand (- 64 shift) 63)) (imms (- 63 shift)))
    (emit32 a (bitor UBFM-BASE (bitor (lsl immr 16) (bitor (lsl imms 10) (bitor (lsl rn 5) rd)))))))
(define (e-cmp a rn rm) (emit32 a (bitor SUBS-REG-BASE (bitor (lsl rm 16) (lsl rn 5)))))
(define (e-cmp-i a rn imm) (emit32 a (bitor SUBS-IMM-BASE (bitor (lsl (bitand imm MASK-12BIT) 10) (lsl rn 5)))))
(define (e-cset a rd cond) (emit32 a (bitor CSINC-BASE (bitor (lsl (bitxor cond 1) 12) rd))))
(define (e-ldr a rt rn imm) (emit32 a (bitor LDR-IMM-BASE (bitor (lsl (bitand (asr imm 3) MASK-12BIT) 10) (bitor (lsl rn 5) rt)))))
(define (e-str a rt rn imm) (emit32 a (bitor STR-IMM-BASE (bitor (lsl (bitand (asr imm 3) MASK-12BIT) 10) (bitor (lsl rn 5) rt)))))
(define (e-ldrb a rt rn imm) (emit32 a (bitor LDRB-IMM-BASE (bitor (lsl (bitand imm MASK-12BIT) 10) (bitor (lsl rn 5) rt)))))
(define (e-strb a rt rn imm) (emit32 a (bitor STRB-IMM-BASE (bitor (lsl (bitand imm MASK-12BIT) 10) (bitor (lsl rn 5) rt)))))
(define (e-ldr-post a rt rn imm) (emit32 a (bitor LDR-POST-BASE (bitor (lsl (bitand imm MASK-9BIT) 12) (bitor (lsl rn 5) rt)))))
(define (e-str-pre a rt rn imm) (emit32 a (bitor STR-PRE-BASE (bitor (lsl (bitand imm MASK-9BIT) 12) (bitor (lsl rn 5) rt)))))
(define (e-stp-pre a r1 r2 rn imm) (emit32 a (bitor STP-PRE-BASE (bitor (lsl (bitand (asr imm 3) MASK-7BIT) 15) (bitor (lsl r2 10) (bitor (lsl rn 5) r1))))))
(define (e-ldp-post a r1 r2 rn imm) (emit32 a (bitor LDP-POST-BASE (bitor (lsl (bitand (asr imm 3) MASK-7BIT) 15) (bitor (lsl r2 10) (bitor (lsl rn 5) r1))))))
(define (e-b a l) (add-fixup a l 0) (emit32 a B-BASE))
(define (e-bl a l) (add-fixup a l 1) (emit32 a BL-BASE))
(define (e-blr a rn) (emit32 a (bitor BLR-BASE (lsl rn 5))))
(define (e-ret a) (emit32 a RET-INSTR))
(define (e-bcond a cond l) (add-fixup a l 2) (emit32 a (bitor BCOND-BASE cond)))
(define (e-svc a imm) (emit32 a (bitor SVC-BASE (lsl imm 5))))
(define (e-nop a) (emit32 a NOP-INSTR))
(define (e-adr a rd l) (add-fixup a l 3) (emit32 a (bitor ADR-BASE rd)))

; Note: e-mov uses ORR which treats reg 31 as XZR, so we use ADD for SP
(define (e-prologue a) (e-stp-pre a X29 X30 SP -16) (e-add-i a X29 SP 0))
(define (e-epilogue a) (e-ldp-post a X29 X30 SP 16) (e-ret a))
(define (e-push-x0 a) (e-str-pre a X0 SP -16))
(define (e-pop-x0 a) (e-ldr-post a X0 SP 16))
(define (e-pop-x1 a) (e-ldr-post a X1 SP 16))
(define (e-alloc a sz) (e-sub-i a SP SP sz))
(define (e-free a sz) (e-add-i a SP SP sz))
(define (e-sys-exit a) (e-mov-imm a X16 SYS-EXIT) (e-svc a 128))
(define (e-sys-write a) (e-mov-imm a X16 SYS-WRITE) (e-svc a 128))
(define (e-sys-read a) (e-mov-imm a X16 SYS-READ) (e-svc a 128))

; Fixups
(define (resolve-fixups a) (resolve-fx-h a (asm-fixups a)))
(define (resolve-fx-h a fx) (if (null? fx) 0 (begin (resolve-one a (car fx)) (resolve-fx-h a (cdr fx)))))
(define (resolve-one a fx)
  (let ((off (car fx)) (lbl (car (cdr fx))) (typ (car (cdr (cdr fx)))))
    (let ((tgt (lookup-label a lbl)) (buf (asm-buf a)))
      (let ((ioff (asr (- tgt off) 2)))
        (let ((inst (bitor (vector-ref buf off) (bitor (lsl (vector-ref buf (+ off 1)) 8) (bitor (lsl (vector-ref buf (+ off 2)) 16) (lsl (vector-ref buf (+ off 3)) 24))))))
          (let ((patched (if (= typ 3)
                            (bitor inst (bitor (lsl (bitand ioff 3) 29) (lsl (bitand (asr ioff 2) MASK-19BIT) 5)))
                            (if (= typ 2)
                                (bitor inst (lsl (bitand ioff MASK-19BIT) 5))
                                (bitor inst (bitand ioff MASK-26BIT))))))
            (vector-set! buf off (bitand patched 255))
            (vector-set! buf (+ off 1) (bitand (asr patched 8) 255))
            (vector-set! buf (+ off 2) (bitand (asr patched 16) 255))
            (vector-set! buf (+ off 3) (bitand (asr patched 24) 255))))))))

; ===== Lexer =====
(define (make-lexer source) (cons source 0))
(define (lexer-eof? lex) (>= (cdr lex) (string-length (car lex))))
(define (lexer-peek lex) (if (lexer-eof? lex) 0 (string-ref (car lex) (cdr lex))))
(define (lexer-advance lex) (cons (car lex) (+ (cdr lex) 1)))

(define (skip-whitespace lex)
  (if (lexer-eof? lex) lex
      (if (char-whitespace? (lexer-peek lex)) (skip-whitespace (lexer-advance lex)) lex)))

(define (skip-comment lex)
  (if (lexer-eof? lex) lex
      (if (= (lexer-peek lex) 10) (lexer-advance lex) (skip-comment (lexer-advance lex)))))

(define (skip-ws lex)
  (let ((lex2 (skip-whitespace lex)))
    (if (lexer-eof? lex2) lex2
        (if (= (lexer-peek lex2) 59) (skip-ws (skip-comment (lexer-advance lex2))) lex2))))

(define (sym-char? c) (or (char-alphabetic? c) (char-numeric? c) (= c 45) (= c 43) (= c 42) (= c 47) (= c 60) (= c 62) (= c 61) (= c 95) (= c 33) (= c 63) (= c 38) (= c 124)))

(define (read-num-h lex acc)
  (if (lexer-eof? lex) (cons acc lex)
      (let ((ch (lexer-peek lex)))
        (if (char-numeric? ch) (read-num-h (lexer-advance lex) (+ (* acc 10) (- ch 48))) (cons acc lex)))))

(define (read-num lex) (read-num-h lex 0))

(define (chars->str-h chars acc) (if (null? chars) acc (chars->str-h (cdr chars) (string-append acc (make-char-string (car chars))))))
(define (chars->str chars) (chars->str-h chars ""))

(define (read-sym-h lex chars)
  (if (lexer-eof? lex) (cons chars lex)
      (let ((ch (lexer-peek lex)))
        (if (sym-char? ch) (read-sym-h (lexer-advance lex) (cons ch chars)) (cons chars lex)))))

(define (read-sym lex) (let ((r (read-sym-h lex (list)))) (cons (chars->str (reverse (car r))) (cdr r))))

(define (read-str-h lex chars)
  (if (lexer-eof? lex) (cons chars lex)
      (let ((ch (lexer-peek lex)))
        (if (= ch 34) (cons chars (lexer-advance lex))
            (if (= ch 92)
                (let ((lex2 (lexer-advance lex)))
                  (if (lexer-eof? lex2) (cons chars lex2)
                      (let ((esc (lexer-peek lex2)))
                        (read-str-h (lexer-advance lex2) (cons (if (= esc 110) 10 (if (= esc 116) 9 esc)) chars)))))
                (read-str-h (lexer-advance lex) (cons ch chars)))))))

(define (read-str lex) (let ((r (read-str-h lex (list)))) (cons (chars->str (reverse (car r))) (cdr r))))

(define (read-tok lex)
  (let ((lex2 (skip-ws lex)))
    (if (lexer-eof? lex2) (list)
        (let ((ch (lexer-peek lex2)))
          (if (= ch 40) (cons (cons 0 "(") (lexer-advance lex2))
              (if (= ch 41) (cons (cons 1 ")") (lexer-advance lex2))
                  (if (= ch 39) (cons (cons 5 "'") (lexer-advance lex2))
                      (if (= ch 34) (let ((r (read-str (lexer-advance lex2)))) (cons (cons 4 (car r)) (cdr r)))
                          (if (char-numeric? ch) (let ((r (read-num lex2))) (cons (cons 2 (car r)) (cdr r)))
                              (if (= ch 45)
                                  (let ((lex3 (lexer-advance lex2)))
                                    (if (and (not (lexer-eof? lex3)) (char-numeric? (lexer-peek lex3)))
                                        (let ((r (read-num lex3))) (cons (cons 2 (- 0 (car r))) (cdr r)))
                                        (let ((r (read-sym lex2))) (cons (cons 3 (car r)) (cdr r)))))
                                  (let ((r (read-sym lex2))) (cons (cons 3 (car r)) (cdr r)))))))))))))

(define (tokenize-h lex toks) (let ((r (read-tok lex))) (if (null? r) (reverse toks) (tokenize-h (cdr r) (cons (car r) toks)))))
(define (tokenize src) (tokenize-h (make-lexer src) (list)))

; ===== Parser =====
(define (tok-type t) (car t))
(define (tok-val t) (cdr t))
(define (make-parser toks) (cons toks 0))
(define (parser-eof? p) (>= (cdr p) (list-length (car p))))
(define (parser-cur p) (if (parser-eof? p) (list) (list-ref (car p) (cdr p))))
(define (parser-adv p) (cons (car p) (+ (cdr p) 1)))

(define (parse-expr p)
  (if (parser-eof? p) (list)
      (let ((tok (parser-cur p)))
        (let ((typ (tok-type tok)) (val (tok-val tok)))
          (if (= typ 0) (parse-list (parser-adv p))
              (if (= typ 1) (list)
                  (if (= typ 2) (cons val (parser-adv p))
                      (if (= typ 3) (cons (cons 3 val) (parser-adv p))
                          (if (= typ 4) (cons (cons 4 val) (parser-adv p))
                              (if (= typ 5)
                                  (let ((q (parse-expr (parser-adv p))))
                                    (if (null? q) (list) (cons (list (cons 3 "quote") (car q)) (cdr q))))
                                  (list)))))))))))

(define (parse-list-h p els)
  (if (parser-eof? p) (list)
      (let ((tok (parser-cur p)))
        (if (= (tok-type tok) 1) (cons (reverse els) (parser-adv p))
            (let ((r (parse-expr p))) (if (null? r) (list) (parse-list-h (cdr r) (cons (car r) els))))))))

(define (parse-list p) (parse-list-h p (list)))

(define (parse-all-h p exprs)
  (if (parser-eof? p) (reverse exprs)
      (let ((r (parse-expr p))) (if (null? r) (reverse exprs) (parse-all-h (cdr r) (cons (car r) exprs))))))

(define (parse toks) (parse-all-h (make-parser toks) (list)))

; ===== Code Generator =====
(define (make-cg)
  (let ((cg (make-vector 8)))
    (vector-set! cg 0 (make-asm))
    (vector-set! cg 1 (list)) (vector-set! cg 2 (list))
    (vector-set! cg 3 (list)) (vector-set! cg 4 0)
    (vector-set! cg 5 0) (vector-set! cg 6 (list))
    (vector-set! cg 7 0)
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

(define (ast-num? n) (not (pair? n)))
(define (ast-sym? n) (and (pair? n) (= (car n) 3)))
(define (ast-str? n) (and (pair? n) (= (car n) 4)))
(define (ast-list? n) (and (pair? n) (not (= (car n) 3)) (not (= (car n) 4))))
(define (ast-sym-name n) (cdr n))
(define (ast-str-val n) (cdr n))

(define (push-temp cg) (e-push-x0 (cg-asm cg)) (cg-set-depth! cg (+ (cg-depth cg) 16)))
(define (pop-to-x0 cg) (e-pop-x0 (cg-asm cg)) (cg-set-depth! cg (- (cg-depth cg) 16)))
(define (pop-to-x1 cg) (e-pop-x1 (cg-asm cg)) (cg-set-depth! cg (- (cg-depth cg) 16)))

(define (lookup-alist key alist)
  (if (null? alist) (list)
      (if (string=? key (car (car alist))) (cdr (car alist)) (lookup-alist key (cdr alist)))))

(define (lookup-func cg name) (lookup-alist name (cg-funcs cg)))
(define (lookup-local cg name) (lookup-alist name (cg-locals cg)))

; Generate expression
(define (gen-expr cg node)
  (if (ast-num? node) (e-mov-imm (cg-asm cg) X0 node)
      (if (ast-sym? node) (gen-sym cg node)
          (if (ast-str? node) (gen-str cg node)
              (if (ast-list? node) (gen-list cg node) (e-mov-imm (cg-asm cg) X0 0))))))

(define (gen-sym cg node)
  (let ((name (ast-sym-name node)))
    (let ((local (lookup-local cg name)))
      (if (not (null? local))
          (e-ldr (cg-asm cg) X0 SP (+ local (cg-depth cg)))
          (e-mov-imm (cg-asm cg) X0 0)))))

(define (gen-str cg node)
  (let ((str (ast-str-val node))
        (lbl (new-label (cg-asm cg) "_str")))
    (cg-set-strings! cg (cons (cons lbl str) (cg-strings cg)))
    (e-adr (cg-asm cg) X0 lbl)))

(define (gen-list cg node)
  (if (null? node) (e-mov-imm (cg-asm cg) X0 0)
      (let ((first (car node)))
        (if (ast-sym? first)
            (let ((op (ast-sym-name first)) (args (cdr node))) (gen-op cg op args))
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
  (if (string=? op "read") (gen-read cg args)
  (if (string=? op "write-byte") (gen-write-byte cg args)
  (if (string=? op "read-byte") (gen-read-byte cg args)
  (if (string=? op "exit") (gen-exit cg args)
  (if (string=? op "string-ref") (gen-string-ref cg args)
  (if (string=? op "string-length") (gen-string-length cg args)
  (if (string=? op "make-vector") (gen-make-vector cg args)
  (if (string=? op "vector-ref") (gen-vector-ref cg args)
  (if (string=? op "vector-set!") (gen-vector-set cg args)
  (gen-call cg op args))))))))))))))))))))))))))))))))))))

(define (gen-arith cg args op)
  (if (null? args) (e-mov-imm (cg-asm cg) X0 0)
      (if (null? (cdr args))
          (begin (gen-expr cg (car args)) (if (= op 1) (e-neg (cg-asm cg) X0 X0) 0))
          (begin (gen-expr cg (car args)) (push-temp cg) (gen-arith-rest cg (cdr args) op)))))

(define (gen-arith-rest cg args op)
  (gen-expr cg (car args))
  (pop-to-x1 cg)
  (let ((a (cg-asm cg)))
    (if (= op 0) (e-add a X0 X1 X0)
        (if (= op 1) (e-sub a X0 X1 X0)
            (if (= op 2) (e-mul a X0 X1 X0)
                (e-sdiv a X0 X1 X0)))))
  (if (not (null? (cdr args))) (begin (push-temp cg) (gen-arith-rest cg (cdr args) op)) 0))

(define (gen-mod cg args)
  (gen-expr cg (car args)) (push-temp cg) (gen-expr cg (car (cdr args))) (pop-to-x1 cg)
  (let ((a (cg-asm cg))) (e-sdiv a X2 X1 X0) (e-msub a X0 X2 X0 X1)))

(define (gen-bitop cg args op)
  (gen-expr cg (car args)) (push-temp cg) (gen-expr cg (car (cdr args))) (pop-to-x1 cg)
  (let ((a (cg-asm cg)))
    (if (= op 0) (e-and a X0 X1 X0) (if (= op 1) (e-orr a X0 X1 X0) (e-eor a X0 X1 X0)))))

(define (gen-bitnot cg args)
  (gen-expr cg (car args)) (e-mov-imm (cg-asm cg) X1 -1) (e-eor (cg-asm cg) X0 X0 X1))

(define (gen-shift cg args op)
  (gen-expr cg (car args)) (push-temp cg) (gen-expr cg (car (cdr args))) (pop-to-x1 cg)
  (let ((a (cg-asm cg)))
    (if (= op 0) (e-lsl-reg a X0 X1 X0) (if (= op 1) (e-lsr-reg a X0 X1 X0) (e-asr-reg a X0 X1 X0)))))

(define (gen-cmp cg args cond)
  (gen-expr cg (car args)) (push-temp cg) (gen-expr cg (car (cdr args))) (pop-to-x1 cg)
  (e-cmp (cg-asm cg) X1 X0) (e-cset (cg-asm cg) X0 cond))

(define (gen-if cg args)
  (let ((a (cg-asm cg)) (else-lbl (new-label (cg-asm cg) "else")) (end-lbl (new-label (cg-asm cg) "end")))
    (gen-expr cg (car args)) (e-cmp-i a X0 0) (e-bcond a COND-EQ else-lbl)
    (gen-expr cg (car (cdr args))) (e-b a end-lbl) (def-label a else-lbl)
    (if (not (null? (cdr (cdr args)))) (gen-expr cg (car (cdr (cdr args)))) (e-mov-imm a X0 0))
    (def-label a end-lbl)))

(define (gen-and cg args)
  (let ((a (cg-asm cg)) (f-lbl (new-label a "f")) (end-lbl (new-label a "e")))
    (gen-and-h cg args f-lbl) (e-mov-imm a X0 1) (e-b a end-lbl)
    (def-label a f-lbl) (e-mov-imm a X0 0) (def-label a end-lbl)))

(define (gen-and-h cg args f-lbl)
  (if (null? args) 0
      (begin (gen-expr cg (car args)) (e-cmp-i (cg-asm cg) X0 0)
             (e-bcond (cg-asm cg) COND-EQ f-lbl) (gen-and-h cg (cdr args) f-lbl))))

(define (gen-or cg args)
  (let ((a (cg-asm cg)) (t-lbl (new-label a "t")) (end-lbl (new-label a "e")))
    (gen-or-h cg args t-lbl) (e-mov-imm a X0 0) (e-b a end-lbl)
    (def-label a t-lbl) (e-mov-imm a X0 1) (def-label a end-lbl)))

(define (gen-or-h cg args t-lbl)
  (if (null? args) 0
      (begin (gen-expr cg (car args)) (e-cmp-i (cg-asm cg) X0 0)
             (e-bcond (cg-asm cg) COND-NE t-lbl) (gen-or-h cg (cdr args) t-lbl))))

(define (gen-not cg args)
  (gen-expr cg (car args)) (e-cmp-i (cg-asm cg) X0 0) (e-cset (cg-asm cg) X0 COND-EQ))

(define (gen-let cg args)
  (let ((bindings (car args)) (body (cdr args)))
    (let ((num (list-length bindings)) (saved-locals (cg-locals cg)) (saved-depth (cg-depth cg)))
      (if (> num 0)
          (let ((stack-sz (* (+ (/ num 2) 1) 16)))
            (e-alloc (cg-asm cg) stack-sz) (cg-set-depth! cg (+ (cg-depth cg) stack-sz))
            (gen-let-bindings cg bindings 0 stack-sz) (gen-begin cg body)
            (e-free (cg-asm cg) stack-sz) (cg-set-depth! cg saved-depth))
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

(define (gen-begin cg args) (if (null? args) 0 (begin (gen-expr cg (car args)) (gen-begin cg (cdr args)))))

(define (gen-write cg args)
  (let ((a (cg-asm cg)))
    (gen-expr cg (car args)) (push-temp cg)
    (gen-expr cg (car (cdr args))) (push-temp cg)
    (gen-expr cg (car (cdr (cdr args))))
    (e-mov a X2 X0) (pop-to-x1 cg) (pop-to-x0 cg) (e-sys-write a)))

(define (gen-read cg args)
  (let ((a (cg-asm cg)))
    (gen-expr cg (car args)) (push-temp cg)
    (gen-expr cg (car (cdr args))) (push-temp cg)
    (gen-expr cg (car (cdr (cdr args))))
    (e-mov a X2 X0) (pop-to-x1 cg) (pop-to-x0 cg) (e-sys-read a)))

; write-byte: writes a single byte to fd
; (write-byte fd byte) -> bytes written
(define (gen-write-byte cg args)
  (let ((a (cg-asm cg)))
    (gen-expr cg (car args)) (push-temp cg)        ; fd
    (gen-expr cg (car (cdr args)))                 ; byte value in X0
    (e-sub-i a SP SP 16) (cg-set-depth! cg (+ (cg-depth cg) 16))  ; allocate stack space
    (e-strb a X0 SP 0)                             ; store byte at sp
    (e-add-i a X1 SP 0)                            ; X1 = pointer to byte (use add for SP)
    (e-mov-imm a X2 1)                             ; X2 = 1 byte
    (e-ldr a X0 SP 16)                             ; X0 = fd from stack
    (e-sys-write a)
    (e-add-i a SP SP 32) (cg-set-depth! cg (- (cg-depth cg) 32))))

; read-byte: reads a single byte from fd
; (read-byte fd) -> byte value or -1 on EOF
(define (gen-read-byte cg args)
  (let ((a (cg-asm cg)))
    (gen-expr cg (car args))                       ; fd in X0
    (e-sub-i a SP SP 16) (cg-set-depth! cg (+ (cg-depth cg) 16))
    (e-add-i a X1 SP 0)                            ; X1 = pointer to buffer (use add for SP)
    (e-mov-imm a X2 1)                             ; X2 = 1 byte
    (e-sys-read a)                                 ; X0 = bytes read
    (e-cmp-i a X0 0)                               ; check if 0 bytes read
    (let ((got-byte (new-label a "gotb")) (done (new-label a "done")))
      (e-bcond a COND-NE got-byte)
      (e-mov-imm a X0 0) (e-sub-i a X0 X0 1)       ; X0 = -1 (EOF)
      (e-add-i a SP SP 16)                         ; restore sp (EOF path)
      (e-b a done)
      (def-label a got-byte)
      (e-ldrb a X0 SP 0)                           ; X0 = the byte
      (e-add-i a SP SP 16)                         ; restore sp (success path)
      (def-label a done)
      (cg-set-depth! cg (- (cg-depth cg) 16)))))

(define (gen-exit cg args) (gen-expr cg (car args)) (e-sys-exit (cg-asm cg)))

(define (gen-string-ref cg args)
  (gen-expr cg (car args)) (push-temp cg)
  (gen-expr cg (car (cdr args))) (pop-to-x1 cg)
  (e-add (cg-asm cg) X0 X1 X0) (e-ldrb (cg-asm cg) X0 X0 0))

(define (gen-string-length cg args)
  (gen-expr cg (car args))
  (let ((a (cg-asm cg)) (loop (new-label (cg-asm cg) "loop")) (done (new-label (cg-asm cg) "done")))
    (e-mov a X1 X0) (e-mov-imm a X0 0)
    (def-label a loop) (e-ldrb a X2 X1 0) (e-cmp-i a X2 0) (e-bcond a COND-EQ done)
    (e-add-i a X0 X0 1) (e-add-i a X1 X1 1) (e-b a loop) (def-label a done)))

; Vector operations - X28 is heap pointer
; Helper to multiply by 8: X0 = X0 * 8 using shifts (3 adds)
(define (e-mul8 a rd rn)
  (e-add a rd rn rn)      ; rd = rn * 2
  (e-add a rd rd rd)      ; rd = rn * 4
  (e-add a rd rd rd))     ; rd = rn * 8

(define (gen-make-vector cg args)
  (let ((a (cg-asm cg)))
    (gen-expr cg (car args))       ; length in X0
    (e-str a X0 X28 0)             ; store length at heap[0]
    (e-mov a X1 X28)               ; X1 = pointer to return
    (e-add-i a X0 X0 1)            ; X0 = length + 1
    (e-mul8 a X0 X0)               ; X0 = (length+1) * 8
    (e-add a X28 X28 X0)           ; advance heap pointer
    (e-mov a X0 X1)))              ; return pointer

(define (gen-vector-ref cg args)
  (let ((a (cg-asm cg)))
    (gen-expr cg (car args)) (push-temp cg)    ; base
    (gen-expr cg (car (cdr args))) (pop-to-x1 cg)  ; index in X0, base in X1
    (e-add-i a X0 X0 1)             ; X0 = index + 1 (skip length field)
    (e-mul8 a X0 X0)                ; X0 = (index+1) * 8
    (e-add a X0 X1 X0)              ; X0 = base + offset
    (e-ldr a X0 X0 0)))             ; X0 = mem[base + offset]

(define (gen-vector-set cg args)
  (let ((a (cg-asm cg)))
    (gen-expr cg (car args)) (push-temp cg)              ; base
    (gen-expr cg (car (cdr args))) (push-temp cg)        ; index
    (gen-expr cg (car (cdr (cdr args))))                 ; value in X0
    (pop-to-x1 cg) (e-add-i a X1 X1 1) (e-mul8 a X1 X1)  ; X1 = (index+1) * 8
    (e-ldr-post a X2 SP 16) (cg-set-depth! cg (- (cg-depth cg) 16))  ; X2 = base
    (e-add a X1 X2 X1)              ; X1 = base + offset
    (e-str a X0 X1 0)))             ; mem[base + offset] = value

(define (gen-call cg name args)
  (let ((func (lookup-func cg name)))
    (if (null? func) (e-mov-imm (cg-asm cg) X0 0)
        (let ((num (list-length args)))
          (gen-push-args cg args) (gen-pop-args cg num 0)
          (e-bl (cg-asm cg) (string-append "_lisp_" name))))))

(define (gen-push-args cg args)
  (if (null? args) 0 (begin (gen-push-args cg (cdr args)) (gen-expr cg (car args)) (push-temp cg))))

(define (gen-pop-args cg num idx)
  (if (>= idx num) 0
      (begin (e-ldr-post (cg-asm cg) idx SP 16) (cg-set-depth! cg (- (cg-depth cg) 16)) (gen-pop-args cg num (+ idx 1)))))

(define (gen-func cg name params body)
  (let ((a (cg-asm cg)))
    (def-label a (string-append "_lisp_" name))
    (e-prologue a) (cg-set-locals! cg (list)) (cg-set-depth! cg 0)
    (let ((num (list-length params)))
      (if (> num 0)
          (let ((stack-sz (* (+ (/ num 2) 1) 16)))
            (e-alloc a stack-sz) (cg-set-lsize! cg stack-sz)
            (store-params cg params 0 stack-sz) (gen-begin cg body) (e-free a stack-sz))
          (gen-begin cg body)))
    (e-epilogue a) (cg-set-locals! cg (list)) (cg-set-depth! cg 0) (cg-set-lsize! cg 0)))

(define (store-params cg params idx stack-sz)
  (if (null? params) 0
      (let ((pname (if (ast-sym? (car params)) (ast-sym-name (car params)) "?"))
            (sp-off (- stack-sz (* (+ idx 1) 8))))
        (e-str (cg-asm cg) idx SP sp-off)
        (cg-set-locals! cg (cons (cons pname (- sp-off (cg-depth cg))) (cg-locals cg)))
        (store-params cg (cdr params) (+ idx 1) stack-sz))))

(define (collect-funcs ast funcs)
  (if (null? ast) funcs
      (let ((node (car ast)))
        (if (ast-list? node)
            (if (and (not (null? node)) (ast-sym? (car node)) (string=? (ast-sym-name (car node)) "define"))
                (if (> (list-length node) 2)
                    (let ((sig (list-ref node 1)))
                      (if (ast-list? sig)
                          (let ((fname (ast-sym-name (car sig))) (params (cdr sig)) (body (cdr (cdr node))))
                            (collect-funcs (cdr ast) (cons (list fname params body) funcs)))
                          (collect-funcs (cdr ast) funcs)))
                    (collect-funcs (cdr ast) funcs))
                (collect-funcs (cdr ast) funcs))
            (collect-funcs (cdr ast) funcs)))))

(define (make-func-alist funcs)
  (if (null? funcs) (list) (let ((f (car funcs))) (cons (cons (car f) (cdr f)) (make-func-alist (cdr funcs))))))

; Compile program
(define (compile-prog cg ast)
  (let ((funcs (collect-funcs ast (list))))
    (cg-set-funcs! cg (make-func-alist funcs))
    (let ((a (cg-asm cg)))
      (e-b a "_main")
      (gen-all-funcs cg funcs)
      (def-label a "_main") (e-prologue a)
      ; Allocate 64KB heap on stack and set X28 to point to it
      (e-sub-i a SP SP 4080)  ; allocate in chunks (max 12-bit immediate)
      (e-sub-i a SP SP 4080)
      (e-sub-i a SP SP 4080)
      (e-sub-i a SP SP 4080)  ; total 16320 bytes (enough for testing)
      (e-add-i a X28 SP 0)    ; X28 = heap start
      (gen-toplevel cg ast) (e-sys-exit a)
      (gen-string-data cg)
      (resolve-fixups a))))

(define (gen-all-funcs cg funcs)
  (if (null? funcs) 0
      (begin (let ((f (car funcs))) (gen-func cg (car f) (car (cdr f)) (car (cdr (cdr f)))))
             (gen-all-funcs cg (cdr funcs)))))

(define (gen-toplevel cg ast)
  (if (null? ast) 0
      (let ((node (car ast)))
        (if (ast-list? node)
            (if (and (not (null? node)) (ast-sym? (car node)) (string=? (ast-sym-name (car node)) "define"))
                (gen-toplevel cg (cdr ast))
                (begin (gen-expr cg node) (gen-toplevel cg (cdr ast))))
            (begin (gen-expr cg node) (gen-toplevel cg (cdr ast)))))))

(define (gen-string-data cg)
  (let ((a (cg-asm cg)) (strings (cg-strings cg)))
    (gen-str-data-h a strings)))

(define (gen-str-data-h a strings)
  (if (null? strings) 0
      (let ((s (car strings)))
        (def-label a (car s))
        (emit-str-bytes a (cdr s) 0)
        (emit-byte a 0)  ; null terminator
        (align-4 a)
        (gen-str-data-h a (cdr strings)))))

(define (emit-str-bytes a str idx)
  (if (>= idx (string-length str)) 0
      (begin (emit-byte a (string-ref str idx)) (emit-str-bytes a str (+ idx 1)))))

(define (align-4 a)
  (let ((pos (asm-pos a)))
    (let ((rem (mod pos 4)))
      (if (= rem 0) 0 (pad-zeros a (- 4 rem))))))

(define (align-8 a)
  (let ((pos (asm-pos a)))
    (let ((rem (mod pos 8)))
      (if (= rem 0) 0 (pad-zeros a (- 8 rem))))))

(define (pad-zeros a count) (if (<= count 0) 0 (begin (emit-byte a 0) (pad-zeros a (- count 1)))))

; ===== Mach-O Generation =====
(define (make-macho)
  (let ((m (make-vector 3)))
    (vector-set! m 0 (make-vector 32768)) (vector-set! m 1 0) (vector-set! m 2 0) m))

(define (macho-buf m) (vector-ref m 0))
(define (macho-pos m) (vector-ref m 1))
(define (macho-set-pos! m p) (vector-set! m 1 p))

(define (emit-byte-m m b) (vector-set! (macho-buf m) (macho-pos m) (bitand b 255)) (macho-set-pos! m (+ (macho-pos m) 1)))
(define (emit32-m m i) (emit-byte-m m (bitand i 255)) (emit-byte-m m (bitand (asr i 8) 255)) (emit-byte-m m (bitand (asr i 16) 255)) (emit-byte-m m (bitand (asr i 24) 255)))
(define (emit64-m m i) (emit32-m m (bitand i 4294967295)) (emit32-m m (bitand (asr i 32) 4294967295)))
(define (emit-str-m m str len) (emit-str-m-h m str 0 (string-length str) len))
(define (emit-str-m-h m str idx slen padlen) (if (>= idx padlen) 0 (begin (if (< idx slen) (emit-byte-m m (string-ref str idx)) (emit-byte-m m 0)) (emit-str-m-h m str (+ idx 1) slen padlen))))
(define (emit-pad-m m align) (let ((rem (mod (macho-pos m) align))) (if (= rem 0) 0 (emit-zeros-m m (- align rem)))))
(define (emit-zeros-m m n) (if (<= n 0) 0 (begin (emit-byte-m m 0) (emit-zeros-m m (- n 1)))))

; Generate Mach-O object file header
; Object file layout:
; 1. Mach header (32 bytes)
; 2. LC_SEGMENT_64 with __text section (152 bytes)
; 3. LC_SYMTAB (24 bytes)
; 4. LC_BUILD_VERSION (24 bytes)
; 5. Code (at offset 232, aligned to 4)
; 6. Symbol table (16 bytes per symbol)
; 7. String table

(define (gen-object-header m code-size)
  (let ((mach-header-size 32)
        (load-cmds-size 200)  ; 152 + 24 + 24
        (ncmds 3))
    ; Code starts at offset 232 (aligned to 8)
    (let ((code-offset 232)
          (symtab-offset (+ 232 code-size))
          (strtab-offset (+ 232 code-size 16)))  ; 16 bytes per nlist_64
      ; Mach header (32 bytes)
      (emit32-m m MH-MAGIC-64)
      (emit32-m m CPU-TYPE-ARM64)
      (emit32-m m 0)  ; cpusubtype
      (emit32-m m MH-OBJECT)  ; filetype = object
      (emit32-m m ncmds)
      (emit32-m m load-cmds-size)
      (emit32-m m MH-SUBSECTIONS-VIA-SYMBOLS)
      (emit32-m m 0)  ; reserved

      ; LC_SEGMENT_64 (152 bytes = 72 header + 80 section)
      (emit32-m m LC-SEGMENT-64)
      (emit32-m m 152)
      (emit-str-m m "" 16)  ; empty segment name for object files
      (emit64-m m 0)  ; vmaddr = 0
      (emit64-m m code-size)  ; vmsize = code size
      (emit64-m m code-offset)  ; fileoff
      (emit64-m m code-size)  ; filesize
      (emit32-m m (bitor VM-PROT-READ VM-PROT-EXECUTE))  ; maxprot
      (emit32-m m (bitor VM-PROT-READ VM-PROT-EXECUTE))  ; initprot
      (emit32-m m 1)  ; nsects
      (emit32-m m 0)  ; flags

      ; __text section (80 bytes)
      (emit-str-m m "__text" 16)
      (emit-str-m m "__TEXT" 16)
      (emit64-m m 0)  ; addr = 0 for object files
      (emit64-m m code-size)
      (emit32-m m code-offset)  ; offset
      (emit32-m m 2)  ; align 2^2 = 4
      (emit32-m m 0)  ; reloff
      (emit32-m m 0)  ; nreloc
      (emit32-m m (bitor S-ATTR-PURE-INSTRUCTIONS S-ATTR-SOME-INSTRUCTIONS))
      (emit32-m m 0)
      (emit32-m m 0)
      (emit32-m m 0)

      ; LC_SYMTAB (24 bytes)
      (emit32-m m LC-SYMTAB)
      (emit32-m m 24)
      (emit32-m m symtab-offset)  ; symoff
      (emit32-m m 1)  ; nsyms = 1 (_main)
      (emit32-m m strtab-offset)  ; stroff
      (emit32-m m 7)  ; strsize (null + "_main" + null)

      ; LC_BUILD_VERSION (24 bytes)
      (emit32-m m LC-BUILD-VERSION)
      (emit32-m m 24)
      (emit32-m m PLATFORM-MACOS)
      (emit32-m m 851968)  ; minos 13.0
      (emit32-m m 851968)  ; sdk 13.0
      (emit32-m m 0)  ; ntools

      ; Pad to code offset (232 bytes)
      (emit-pad-m m code-offset))))

; Generate symbol table entry (nlist_64 = 16 bytes)
(define (emit-symbol m)
  (emit32-m m 1)  ; n_strx: offset 1 in string table (after leading null)
  (emit-byte-m m (bitor N-SECT N-EXT))  ; n_type: external, defined in section
  (emit-byte-m m 1)  ; n_sect: section 1 (__text)
  (emit-byte-m m 0)  ; n_desc low
  (emit-byte-m m 0)  ; n_desc high
  (emit64-m m 0))    ; n_value: 0 for object files

; Generate string table
(define (emit-strtab m)
  (emit-byte-m m 0)    ; leading null
  (emit-byte-m m 95)   ; '_'
  (emit-byte-m m 109)  ; 'm'
  (emit-byte-m m 97)   ; 'a'
  (emit-byte-m m 105)  ; 'i'
  (emit-byte-m m 110)  ; 'n'
  (emit-byte-m m 0))   ; null terminator

(define (gen-binary cg)
  (let ((a (cg-asm cg)) (m (make-macho)))
    (let ((code-size (asm-pos a)))
      ; Generate object file header
      (gen-object-header m code-size)
      ; Copy code
      (copy-code-m m (asm-buf a) code-size)
      ; Emit symbol table
      (emit-symbol m)
      ; Emit string table
      (emit-strtab m)
      m)))

(define (copy-code-m m buf size) (copy-code-m-h m buf 0 size))
(define (copy-code-m-h m buf idx size) (if (>= idx size) 0 (begin (emit-byte-m m (vector-ref buf idx)) (copy-code-m-h m buf (+ idx 1) size))))

; ===== Output =====
(define (output-binary m)
  (let ((buf (macho-buf m)) (len (macho-pos m)))
    (print len) (output-bin-h buf 0 len)))

(define (output-bin-h buf idx len) (if (>= idx len) 0 (begin (print (vector-ref buf idx)) (output-bin-h buf (+ idx 1) len))))

; ===== Main =====
; Test: param with let using read-byte
(define source "
(define (print-char ch) (write-byte 1 ch))

(define (print-num n)
  (if (< n 10) (print-char (+ n 48))
      (begin (print-num (/ n 10)) (print-char (+ (- n (* (/ n 10) 10)) 48)))))

(define (mytest x)
  (let ((ch (read-byte 0)))
    (if (= ch 0) 0 (print-num ch))))

(mytest 0)
(print-char 10)
")
(define toks (tokenize source))
(define ast (parse toks))
(define cg (make-cg))
(compile-prog cg ast)
(define m (gen-binary cg))
(output-binary m)
