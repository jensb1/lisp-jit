; Full Bootstrap Test
; This uses the complete Lisp compiler to compile a program with functions

; ===== Include the full bootstrap compiler =====

; Register constants
(define X0 0)
(define X1 1)
(define X2 2)
(define X9 9)
(define X10 10)
(define X11 11)
(define X12 12)
(define X13 13)
(define X14 14)
(define X28 28)
(define X29 29)
(define X30 30)
(define SP 31)
(define XZR 31)

; Condition codes
(define COND-EQ 0)
(define COND-NE 1)
(define COND-GE 10)
(define COND-LT 11)
(define COND-GT 12)
(define COND-LE 13)

; ARM64 opcodes
(define MOVZ-BASE 3531603968)
(define MOVN-BASE 2457862144)
(define MOVK-BASE 4068474880)
(define ORR-BASE 2852127712)
(define ADD-REG-BASE 2332033024)
(define ADD-IMM-BASE 2432696320)
(define SUB-REG-BASE 3405774848)
(define SUB-IMM-BASE 3506438144)
(define MUL-BASE 2600500224)
(define SDIV-BASE 2596277248)
(define UDIV-BASE 2596276224)
(define MSUB-BASE 2600501248)
(define SUBS-REG-BASE 3942645791)
(define SUBS-IMM-BASE 4043309087)
(define CSINC-BASE 2594113504)
(define LDR-IMM-BASE 4181721088)
(define STR-IMM-BASE 4177526784)
(define LDR-POST-BASE 4164944896)
(define STR-PRE-BASE 4160752640)
(define STRB-BASE 956301312)
(define STP-PRE-BASE 2843738112)
(define LDP-POST-BASE 2831155200)
(define B-BASE 335544320)
(define BL-BASE 2483027968)
(define BLR-BASE 3594452992)
(define RET-INSTR 3596551104)
(define BCOND-BASE 1409286144)

; Masks
(define MASK-16BIT 65535)
(define MASK-12BIT 4095)
(define MASK-9BIT 511)
(define MASK-7BIT 127)
(define MASK-26BIT 67108863)
(define MASK-19BIT 524287)

; Helpers
(define (list-length lst) (if (null? lst) 0 (+ 1 (list-length (cdr lst)))))
(define (list-ref lst n) (if (= n 0) (car lst) (list-ref (cdr lst) (- n 1))))
(define (reverse-h lst acc) (if (null? lst) acc (reverse-h (cdr lst) (cons (car lst) acc))))
(define (reverse lst) (reverse-h lst (list)))

; Lexer
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
      (= c 95) (= c 33) (= c 63)))

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

(define (read-token lex)
  (let ((lex2 (skip-ws-comments lex)))
    (if (lexer-eof? lex2) (list)
        (let ((ch (lexer-peek lex2)))
          (if (= ch 40) (cons (make-token 0 "(") (lexer-advance lex2))
              (if (= ch 41) (cons (make-token 1 ")") (lexer-advance lex2))
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
                            (cons (make-token 3 (car r)) (cdr r)))))))))))

(define (tokenize-h lex tokens)
  (let ((result (read-token lex)))
    (if (null? result) (reverse tokens)
        (tokenize-h (cdr result) (cons (car result) tokens)))))

(define (tokenize source) (tokenize-h (make-lexer source) (list)))

; Parser
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
                          (list)))))))))

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

; Assembler
(define (make-asm)
  (let ((state (make-vector 5)))
    (vector-set! state 0 (make-vector 65536))
    (vector-set! state 1 0)
    (vector-set! state 2 (list))
    (vector-set! state 3 (list))
    (vector-set! state 4 0)
    state))

(define (asm-buffer asm) (vector-ref asm 0))
(define (asm-pos asm) (vector-ref asm 1))
(define (asm-labels asm) (vector-ref asm 2))
(define (asm-fixups asm) (vector-ref asm 3))
(define (asm-label-ctr asm) (vector-ref asm 4))
(define (asm-set-pos! asm pos) (vector-set! asm 1 pos))
(define (asm-set-labels! asm labels) (vector-set! asm 2 labels))
(define (asm-set-fixups! asm fixups) (vector-set! asm 3 fixups))
(define (asm-set-label-ctr! asm n) (vector-set! asm 4 n))

(define (emit-byte asm byte)
  (vector-set! (asm-buffer asm) (asm-pos asm) (bitand byte 255))
  (asm-set-pos! asm (+ (asm-pos asm) 1)))

(define (emit32 asm instr)
  (emit-byte asm (bitand instr 255))
  (emit-byte asm (bitand (asr instr 8) 255))
  (emit-byte asm (bitand (asr instr 16) 255))
  (emit-byte asm (bitand (asr instr 24) 255)))

(define (new-label asm prefix)
  (let ((n (asm-label-ctr asm)))
    (asm-set-label-ctr! asm (+ n 1))
    (string-append prefix (number->string n))))

(define (define-label asm name)
  (asm-set-labels! asm (cons (cons name (asm-pos asm)) (asm-labels asm))))

(define (add-fixup asm label type)
  (asm-set-fixups! asm (cons (list (asm-pos asm) label type) (asm-fixups asm))))

(define (lookup-label-h labels name)
  (if (null? labels) 0
      (if (string=? (car (car labels)) name)
          (cdr (car labels))
          (lookup-label-h (cdr labels) name))))

(define (lookup-label asm name) (lookup-label-h (asm-labels asm) name))

; Instructions
(define (emit-movz asm rd imm)
  (emit32 asm (bitor MOVZ-BASE (bitor (lsl imm 5) rd))))

(define (emit-movn asm rd imm)
  (emit32 asm (bitor MOVN-BASE (bitor (lsl imm 5) rd))))

(define (emit-movk asm rd imm shift)
  (let ((hw (asr shift 4)))
    (emit32 asm (bitor MOVK-BASE (bitor (lsl hw 21) (bitor (lsl (bitand imm MASK-16BIT) 5) rd))))))

(define (emit-mov-imm asm rd imm)
  (if (and (>= imm 0) (< imm 65536))
      (emit-movz asm rd imm)
      (if (and (< imm 0) (> imm -65536))
          (emit-movn asm rd (bitand (bitnot imm) MASK-16BIT))
          (begin
            (emit-movz asm rd (bitand imm MASK-16BIT))
            (let ((i16 (bitand (asr imm 16) MASK-16BIT)))
              (if (not (= i16 0)) (emit-movk asm rd i16 16) 0))
            (let ((i32 (bitand (asr imm 32) MASK-16BIT)))
              (if (not (= i32 0)) (emit-movk asm rd i32 32) 0))
            (let ((i48 (bitand (asr imm 48) MASK-16BIT)))
              (if (not (= i48 0)) (emit-movk asm rd i48 48) 0))))))

(define (emit-mov-reg asm rd rm)
  (emit32 asm (bitor ORR-BASE (bitor (lsl rm 16) rd))))

(define (emit-add-reg asm rd rn rm)
  (emit32 asm (bitor ADD-REG-BASE (bitor (lsl rm 16) (bitor (lsl rn 5) rd)))))

(define (emit-add-imm asm rd rn imm)
  (emit32 asm (bitor ADD-IMM-BASE (bitor (lsl (bitand imm MASK-12BIT) 10) (bitor (lsl rn 5) rd)))))

(define (emit-sub-reg asm rd rn rm)
  (emit32 asm (bitor SUB-REG-BASE (bitor (lsl rm 16) (bitor (lsl rn 5) rd)))))

(define (emit-sub-imm asm rd rn imm)
  (emit32 asm (bitor SUB-IMM-BASE (bitor (lsl (bitand imm MASK-12BIT) 10) (bitor (lsl rn 5) rd)))))

(define (emit-mul asm rd rn rm)
  (emit32 asm (bitor MUL-BASE (bitor (lsl rm 16) (bitor (lsl rn 5) rd)))))

(define (emit-sdiv asm rd rn rm)
  (emit32 asm (bitor SDIV-BASE (bitor (lsl rm 16) (bitor (lsl rn 5) rd)))))

(define (emit-neg asm rd rm) (emit-sub-reg asm rd XZR rm))

(define (emit-cmp-reg asm rn rm)
  (emit32 asm (bitor SUBS-REG-BASE (bitor (lsl rm 16) (lsl rn 5)))))

(define (emit-cmp-imm asm rn imm)
  (emit32 asm (bitor SUBS-IMM-BASE (bitor (lsl (bitand imm MASK-12BIT) 10) (lsl rn 5)))))

(define (emit-cset asm rd cond)
  (emit32 asm (bitor CSINC-BASE (bitor (lsl (bitxor cond 1) 12) rd))))

(define (emit-ldr-imm asm rt rn imm)
  (let ((scaled (asr imm 3)))
    (emit32 asm (bitor LDR-IMM-BASE (bitor (lsl (bitand scaled MASK-12BIT) 10) (bitor (lsl rn 5) rt))))))

(define (emit-str-imm asm rt rn imm)
  (let ((scaled (asr imm 3)))
    (emit32 asm (bitor STR-IMM-BASE (bitor (lsl (bitand scaled MASK-12BIT) 10) (bitor (lsl rn 5) rt))))))

(define (emit-ldr-post asm rt rn imm)
  (emit32 asm (bitor LDR-POST-BASE (bitor (lsl (bitand imm MASK-9BIT) 12) (bitor (lsl rn 5) rt)))))

(define (emit-str-pre asm rt rn imm)
  (emit32 asm (bitor STR-PRE-BASE (bitor (lsl (bitand imm MASK-9BIT) 12) (bitor (lsl rn 5) rt)))))

(define (emit-stp-pre asm rt1 rt2 rn imm)
  (let ((scaled (bitand (asr imm 3) MASK-7BIT)))
    (emit32 asm (bitor STP-PRE-BASE (bitor (lsl scaled 15) (bitor (lsl rt2 10) (bitor (lsl rn 5) rt1)))))))

(define (emit-ldp-post asm rt1 rt2 rn imm)
  (let ((scaled (bitand (asr imm 3) MASK-7BIT)))
    (emit32 asm (bitor LDP-POST-BASE (bitor (lsl scaled 15) (bitor (lsl rt2 10) (bitor (lsl rn 5) rt1)))))))

(define (emit-b asm label) (add-fixup asm label 0) (emit32 asm B-BASE))
(define (emit-bl asm label) (add-fixup asm label 1) (emit32 asm BL-BASE))
(define (emit-ret asm) (emit32 asm RET-INSTR))
(define (emit-bcond asm cond label) (add-fixup asm label 2) (emit32 asm (bitor BCOND-BASE cond)))

(define (emit-push-x0 asm) (emit-str-pre asm X0 SP -16))
(define (emit-pop-x0 asm) (emit-ldr-post asm X0 SP 16))
(define (emit-pop-x1 asm) (emit-ldr-post asm X1 SP 16))
(define (emit-prologue asm) (emit-stp-pre asm X29 X30 SP -16) (emit-mov-reg asm X29 SP))
(define (emit-epilogue asm) (emit-ldp-post asm X29 X30 SP 16) (emit-ret asm))
(define (emit-alloc-stack asm size) (emit-sub-imm asm SP SP size))
(define (emit-free-stack asm size) (emit-add-imm asm SP SP size))

; Fixups
(define (resolve-fixups asm) (resolve-fixups-h asm (asm-fixups asm)))

(define (resolve-fixups-h asm fixups)
  (if (null? fixups) 0
      (begin (resolve-one-fixup asm (car fixups)) (resolve-fixups-h asm (cdr fixups)))))

(define (resolve-one-fixup asm fixup)
  (let ((offset (car fixup))
        (label (car (cdr fixup)))
        (type (car (cdr (cdr fixup)))))
    (let ((target (lookup-label asm label))
          (buf (asm-buffer asm)))
      (let ((ioff (asr (- target offset) 2)))
        (let ((instr (bitor (vector-ref buf offset)
                           (bitor (lsl (vector-ref buf (+ offset 1)) 8)
                                 (bitor (lsl (vector-ref buf (+ offset 2)) 16)
                                       (lsl (vector-ref buf (+ offset 3)) 24))))))
          (let ((patched (if (= type 2)
                            (bitor instr (lsl (bitand ioff MASK-19BIT) 5))
                            (bitor instr (bitand ioff MASK-26BIT)))))
            (vector-set! buf offset (bitand patched 255))
            (vector-set! buf (+ offset 1) (bitand (asr patched 8) 255))
            (vector-set! buf (+ offset 2) (bitand (asr patched 16) 255))
            (vector-set! buf (+ offset 3) (bitand (asr patched 24) 255))))))))

; Code Generator
(define (make-codegen)
  (let ((cg (make-vector 6)))
    (vector-set! cg 0 (make-asm))
    (vector-set! cg 1 (list))
    (vector-set! cg 2 (list))
    (vector-set! cg 3 (list))
    (vector-set! cg 4 0)
    (vector-set! cg 5 0)
    cg))

(define (cg-asm cg) (vector-ref cg 0))
(define (cg-functions cg) (vector-ref cg 1))
(define (cg-locals cg) (vector-ref cg 3))
(define (cg-temp-depth cg) (vector-ref cg 4))
(define (cg-set-functions! cg f) (vector-set! cg 1 f))
(define (cg-set-locals! cg l) (vector-set! cg 3 l))
(define (cg-set-temp-depth! cg d) (vector-set! cg 4 d))

; AST helpers
(define (ast-number? n) (not (pair? n)))
(define (ast-symbol? n) (and (pair? n) (= (car n) 3)))
(define (ast-list? n) (and (pair? n) (not (= (car n) 3))))
(define (ast-symbol-name n) (cdr n))

; Codegen helpers
(define (push-temp cg) (emit-push-x0 (cg-asm cg)) (cg-set-temp-depth! cg (+ (cg-temp-depth cg) 16)))
(define (pop-to-x0 cg) (emit-pop-x0 (cg-asm cg)) (cg-set-temp-depth! cg (- (cg-temp-depth cg) 16)))
(define (pop-to-x1 cg) (emit-pop-x1 (cg-asm cg)) (cg-set-temp-depth! cg (- (cg-temp-depth cg) 16)))

(define (lookup-alist key alist)
  (if (null? alist) (list)
      (if (string=? key (car (car alist))) (cdr (car alist))
          (lookup-alist key (cdr alist)))))

(define (lookup-function cg name) (lookup-alist name (cg-functions cg)))
(define (lookup-local cg name) (lookup-alist name (cg-locals cg)))

(define (generate-expr cg node)
  (if (ast-number? node) (emit-mov-imm (cg-asm cg) X0 node)
      (if (ast-symbol? node) (generate-symbol cg node)
          (if (ast-list? node) (generate-list cg node) 0))))

(define (generate-symbol cg node)
  (let ((name (ast-symbol-name node)))
    (let ((local (lookup-local cg name)))
      (if (not (null? local))
          (emit-ldr-imm (cg-asm cg) X0 SP (+ local (cg-temp-depth cg)))
          (emit-mov-imm (cg-asm cg) X0 0)))))

(define (generate-list cg node)
  (if (null? node) (emit-mov-imm (cg-asm cg) X0 0)
      (let ((first (car node)))
        (if (ast-symbol? first)
            (let ((op (ast-symbol-name first)) (args (cdr node)))
              (generate-op cg op args))
            (emit-mov-imm (cg-asm cg) X0 0)))))

(define (generate-op cg op args)
  (if (string=? op "+") (generate-arith cg args 0)
      (if (string=? op "-") (generate-arith cg args 1)
          (if (string=? op "*") (generate-arith cg args 2)
              (if (string=? op "/") (generate-arith cg args 3)
                  (if (string=? op "<") (generate-cmp cg args COND-LT)
                      (if (string=? op ">") (generate-cmp cg args COND-GT)
                          (if (string=? op "=") (generate-cmp cg args COND-EQ)
                              (if (string=? op "if") (generate-if cg args)
                                  (if (string=? op "begin") (generate-begin cg args)
                                      (if (string=? op "let") (generate-let cg args)
                                          (if (string=? op "define") 0
                                              (generate-call cg op args)))))))))))))

(define (generate-arith cg args operation)
  (if (null? args) (emit-mov-imm (cg-asm cg) X0 0)
      (if (null? (cdr args))
          (begin
            (generate-expr cg (car args))
            (if (= operation 1) (emit-neg (cg-asm cg) X0 X0) 0))
          (begin
            (generate-expr cg (car args))
            (push-temp cg)
            (generate-arith-rest cg (cdr args) operation)))))

(define (generate-arith-rest cg args operation)
  (generate-expr cg (car args))
  (pop-to-x1 cg)
  (if (= operation 0) (emit-add-reg (cg-asm cg) X0 X1 X0)
      (if (= operation 1) (emit-sub-reg (cg-asm cg) X0 X1 X0)
          (if (= operation 2) (emit-mul (cg-asm cg) X0 X1 X0)
              (emit-sdiv (cg-asm cg) X0 X1 X0))))
  (if (not (null? (cdr args)))
      (begin (push-temp cg) (generate-arith-rest cg (cdr args) operation))
      0))

(define (generate-cmp cg args cond)
  (generate-expr cg (car args))
  (push-temp cg)
  (generate-expr cg (car (cdr args)))
  (pop-to-x1 cg)
  (emit-cmp-reg (cg-asm cg) X1 X0)
  (emit-cset (cg-asm cg) X0 cond))

(define (generate-if cg args)
  (let ((asm (cg-asm cg))
        (else-lbl (new-label asm "else"))
        (end-lbl (new-label asm "endif")))
    (generate-expr cg (car args))
    (emit-cmp-imm asm X0 0)
    (emit-bcond asm COND-EQ else-lbl)
    (generate-expr cg (car (cdr args)))
    (emit-b asm end-lbl)
    (define-label asm else-lbl)
    (if (not (null? (cdr (cdr args))))
        (generate-expr cg (car (cdr (cdr args))))
        (emit-mov-imm asm X0 0))
    (define-label asm end-lbl)))

(define (generate-begin cg args)
  (if (null? args) 0
      (begin (generate-expr cg (car args)) (generate-begin cg (cdr args)))))

(define (generate-let cg args)
  (let ((bindings (car args)) (body (cdr args)))
    (let ((num (list-length bindings))
          (saved-locals (cg-locals cg))
          (saved-depth (cg-temp-depth cg)))
      (if (> num 0)
          (let ((stack-sz (* (+ (/ num 2) 1) 16)))
            (emit-alloc-stack (cg-asm cg) stack-sz)
            (cg-set-temp-depth! cg (+ (cg-temp-depth cg) stack-sz))
            (generate-let-bindings cg bindings 0 stack-sz)
            (generate-begin cg body)
            (emit-free-stack (cg-asm cg) stack-sz)
            (cg-set-temp-depth! cg saved-depth))
          (generate-begin cg body))
      (cg-set-locals! cg saved-locals))))

(define (generate-let-bindings cg bindings idx stack-sz)
  (if (null? bindings) 0
      (let ((binding (car bindings)))
        (let ((var (ast-symbol-name (car binding))) (val (car (cdr binding))))
          (generate-expr cg val)
          (let ((sp-off (- stack-sz (* (+ idx 1) 8))))
            (emit-str-imm (cg-asm cg) X0 SP sp-off)
            (cg-set-locals! cg (cons (cons var (- sp-off (cg-temp-depth cg))) (cg-locals cg))))
          (generate-let-bindings cg (cdr bindings) (+ idx 1) stack-sz)))))

(define (generate-call cg name args)
  (let ((func (lookup-function cg name)))
    (if (null? func) (emit-mov-imm (cg-asm cg) X0 0)
        (let ((num (list-length args)))
          (generate-push-args cg args)
          (generate-pop-args cg num 0)
          (emit-bl (cg-asm cg) (string-append "_lisp_" name))))))

(define (generate-push-args cg args)
  (if (null? args) 0
      (begin (generate-push-args cg (cdr args)) (generate-expr cg (car args)) (push-temp cg))))

(define (generate-pop-args cg num idx)
  (if (>= idx num) 0
      (begin
        (emit-ldr-post (cg-asm cg) idx SP 16)
        (cg-set-temp-depth! cg (- (cg-temp-depth cg) 16))
        (generate-pop-args cg num (+ idx 1)))))

; Generate function
(define (generate-function cg name params body)
  (let ((asm (cg-asm cg)))
    (define-label asm (string-append "_lisp_" name))
    (emit-prologue asm)
    (cg-set-locals! cg (list))
    (cg-set-temp-depth! cg 0)
    (let ((num-params (list-length params)))
      (if (> num-params 0)
          (let ((stack-sz (* (+ (/ num-params 2) 1) 16)))
            (emit-alloc-stack asm stack-sz)
            (store-params cg params 0 stack-sz)
            (generate-begin cg body)
            (emit-free-stack asm stack-sz))
          (generate-begin cg body)))
    (emit-epilogue asm)
    (cg-set-locals! cg (list))
    (cg-set-temp-depth! cg 0)))

(define (store-params cg params idx stack-sz)
  (if (null? params) 0
      (let ((param-name (if (ast-symbol? (car params)) (ast-symbol-name (car params)) "?"))
            (sp-off (- stack-sz (* (+ idx 1) 8))))
        (emit-str-imm (cg-asm cg) idx SP sp-off)
        (cg-set-locals! cg (cons (cons param-name sp-off) (cg-locals cg)))
        (store-params cg (cdr params) (+ idx 1) stack-sz))))

; Collect definitions
(define (collect-defs ast funcs)
  (if (null? ast) funcs
      (let ((node (car ast)))
        (if (ast-list? node)
            (if (and (not (null? node)) (ast-symbol? (car node)) (string=? (ast-symbol-name (car node)) "define"))
                (if (> (list-length node) 2)
                    (let ((name-or-sig (list-ref node 1)))
                      (if (ast-list? name-or-sig)
                          (let ((fname (ast-symbol-name (car name-or-sig)))
                                (params (cdr name-or-sig))
                                (body (cdr (cdr node))))
                            (collect-defs (cdr ast) (cons (list fname params body) funcs)))
                          (collect-defs (cdr ast) funcs)))
                    (collect-defs (cdr ast) funcs))
                (collect-defs (cdr ast) funcs))
            (collect-defs (cdr ast) funcs)))))

(define (make-func-alist funcs)
  (if (null? funcs) (list)
      (let ((f (car funcs)))
        (cons (cons (car f) (cdr f)) (make-func-alist (cdr funcs))))))

; Compile program
(define (compile-program cg ast)
  (let ((funcs (collect-defs ast (list))))
    (cg-set-functions! cg (make-func-alist funcs))
    (let ((asm (cg-asm cg)))
      ; Jump to main
      (emit-b asm "_main")
      ; Generate user functions
      (generate-user-functions cg funcs)
      ; Generate main
      (define-label asm "_main")
      (emit-prologue asm)
      (generate-toplevel cg ast)
      (emit-epilogue asm)
      ; Resolve fixups
      (resolve-fixups asm))))

(define (generate-user-functions cg funcs)
  (if (null? funcs) 0
      (begin
        (let ((f (car funcs)))
          (generate-function cg (car f) (car (cdr f)) (car (cdr (cdr f)))))
        (generate-user-functions cg (cdr funcs)))))

(define (generate-toplevel cg ast)
  (if (null? ast) 0
      (let ((node (car ast)))
        (if (ast-list? node)
            (if (and (not (null? node)) (ast-symbol? (car node)) (string=? (ast-symbol-name (car node)) "define"))
                (generate-toplevel cg (cdr ast))
                (begin (generate-expr cg node) (generate-toplevel cg (cdr ast))))
            (begin (generate-expr cg node) (generate-toplevel cg (cdr ast)))))))

; Output bytes
(define (output-bytes cg)
  (let ((asm (cg-asm cg)))
    (let ((len (asm-pos asm)))
      (print len)
      (output-bytes-h (asm-buffer asm) 0 len))))

(define (output-bytes-h buf idx len)
  (if (>= idx len) 0
      (begin (print (vector-ref buf idx)) (output-bytes-h buf (+ idx 1) len))))

; ===== Test: Compile a program with a function =====

(define test-source "(define (double x) (* x 2)) (double 21)")
(define test-tokens (tokenize test-source))
(define test-ast (parse test-tokens))

(define cg (make-codegen))
(compile-program cg test-ast)
(output-bytes cg)
