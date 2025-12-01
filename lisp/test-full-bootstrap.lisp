; Test the full bootstrap compiler pipeline
; Compiles: (define (square x) (* x x)) (square 6) -> should return 36

; ===== Register Constants =====
(define X0 0) (define X1 1) (define X2 2)
(define X9 9) (define X10 10) (define X11 11)
(define X12 12) (define X13 13) (define X14 14)
(define X28 28) (define X29 29) (define X30 30)
(define SP 31) (define XZR 31)

(define COND-EQ 0) (define COND-NE 1)
(define COND-GE 10) (define COND-LT 11)
(define COND-GT 12) (define COND-LE 13)

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
(define SUBS-IMM-BASE 4043309087)
(define CSINC-BASE 2594113504)
(define LDR-IMM-BASE 4181721088)
(define STR-IMM-BASE 4177526784)
(define LDR-POST-BASE 4164944896)
(define STR-PRE-BASE 4160752640)
(define STP-PRE-BASE 2843738112)
(define LDP-POST-BASE 2831155200)
(define B-BASE 335544320)
(define BL-BASE 2483027968)
(define RET-INSTR 3596551104)
(define BCOND-BASE 1409286144)

(define MASK-16BIT 65535)
(define MASK-12BIT 4095)
(define MASK-9BIT 511)
(define MASK-7BIT 127)
(define MASK-26BIT 67108863)
(define MASK-19BIT 524287)

; ===== Utilities =====
(define (list-length lst) (if (null? lst) 0 (+ 1 (list-length (cdr lst)))))
(define (list-ref lst n) (if (= n 0) (car lst) (list-ref (cdr lst) (- n 1))))
(define (reverse-h lst acc) (if (null? lst) acc (reverse-h (cdr lst) (cons (car lst) acc))))
(define (reverse lst) (reverse-h lst (list)))

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
                      (let ((r (read-symbol lex2)))
                        (cons (make-token 3 (car r)) (cdr r))))))))))

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

; ===== Assembler =====
(define (make-asm)
  (let ((s (make-vector 5)))
    (vector-set! s 0 (make-vector 8192))
    (vector-set! s 1 0)
    (vector-set! s 2 (list))
    (vector-set! s 3 (list))
    (vector-set! s 4 0)
    s))

(define (asm-buf a) (vector-ref a 0))
(define (asm-pos a) (vector-ref a 1))
(define (asm-labels a) (vector-ref a 2))
(define (asm-fixups a) (vector-ref a 3))
(define (asm-ctr a) (vector-ref a 4))
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

; Instructions
(define (e-movz a rd im) (emit32 a (bitor MOVZ-BASE (bitor (lsl im 5) rd))))
(define (e-mov a rd rm) (emit32 a (bitor ORR-BASE (bitor (lsl rm 16) rd))))
(define (e-add a rd rn rm) (emit32 a (bitor ADD-REG-BASE (bitor (lsl rm 16) (bitor (lsl rn 5) rd)))))
(define (e-sub a rd rn rm) (emit32 a (bitor SUB-REG-BASE (bitor (lsl rm 16) (bitor (lsl rn 5) rd)))))
(define (e-mul a rd rn rm) (emit32 a (bitor MUL-BASE (bitor (lsl rm 16) (bitor (lsl rn 5) rd)))))
(define (e-sub-i a rd rn im) (emit32 a (bitor SUB-IMM-BASE (bitor (lsl (bitand im MASK-12BIT) 10) (bitor (lsl rn 5) rd)))))
(define (e-add-i a rd rn im) (emit32 a (bitor ADD-IMM-BASE (bitor (lsl (bitand im MASK-12BIT) 10) (bitor (lsl rn 5) rd)))))
(define (e-ldr a rt rn im) (let ((sc (asr im 3))) (emit32 a (bitor LDR-IMM-BASE (bitor (lsl (bitand sc MASK-12BIT) 10) (bitor (lsl rn 5) rt))))))
(define (e-str a rt rn im) (let ((sc (asr im 3))) (emit32 a (bitor STR-IMM-BASE (bitor (lsl (bitand sc MASK-12BIT) 10) (bitor (lsl rn 5) rt))))))
(define (e-ldr-post a rt rn im) (emit32 a (bitor LDR-POST-BASE (bitor (lsl (bitand im MASK-9BIT) 12) (bitor (lsl rn 5) rt)))))
(define (e-str-pre a rt rn im) (emit32 a (bitor STR-PRE-BASE (bitor (lsl (bitand im MASK-9BIT) 12) (bitor (lsl rn 5) rt)))))
(define (e-stp-pre a r1 r2 rn im) (let ((sc (bitand (asr im 3) MASK-7BIT))) (emit32 a (bitor STP-PRE-BASE (bitor (lsl sc 15) (bitor (lsl r2 10) (bitor (lsl rn 5) r1)))))))
(define (e-ldp-post a r1 r2 rn im) (let ((sc (bitand (asr im 3) MASK-7BIT))) (emit32 a (bitor LDP-POST-BASE (bitor (lsl sc 15) (bitor (lsl r2 10) (bitor (lsl rn 5) r1)))))))
(define (e-b a l) (add-fixup a l 0) (emit32 a B-BASE))
(define (e-bl a l) (add-fixup a l 1) (emit32 a BL-BASE))
(define (e-ret a) (emit32 a RET-INSTR))
(define (e-bcond a c l) (add-fixup a l 2) (emit32 a (bitor BCOND-BASE c)))

(define (e-prologue a) (e-stp-pre a X29 X30 SP -16) (e-mov a X29 SP))
(define (e-epilogue a) (e-ldp-post a X29 X30 SP 16) (e-ret a))
(define (e-push-x0 a) (e-str-pre a X0 SP -16))
(define (e-pop-x0 a) (e-ldr-post a X0 SP 16))
(define (e-pop-x1 a) (e-ldr-post a X1 SP 16))
(define (e-alloc a sz) (e-sub-i a SP SP sz))
(define (e-free a sz) (e-add-i a SP SP sz))

; Fixups
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

; ===== Code Generator =====
(define (make-cg)
  (let ((cg (make-vector 5)))
    (vector-set! cg 0 (make-asm))
    (vector-set! cg 1 (list))   ; functions alist
    (vector-set! cg 2 (list))   ; locals alist
    (vector-set! cg 3 0)        ; temp stack depth
    (vector-set! cg 4 0)        ; local stack size
    cg))

(define (cg-asm cg) (vector-ref cg 0))
(define (cg-funcs cg) (vector-ref cg 1))
(define (cg-locals cg) (vector-ref cg 2))
(define (cg-depth cg) (vector-ref cg 3))
(define (cg-lsize cg) (vector-ref cg 4))
(define (cg-set-funcs! cg f) (vector-set! cg 1 f))
(define (cg-set-locals! cg l) (vector-set! cg 2 l))
(define (cg-set-depth! cg d) (vector-set! cg 3 d))
(define (cg-set-lsize! cg s) (vector-set! cg 4 s))

; AST helpers
(define (ast-number? n) (not (pair? n)))
(define (ast-symbol? n) (and (pair? n) (= (car n) 3)))
(define (ast-list? n) (and (pair? n) (not (= (car n) 3))))
(define (ast-sym-name n) (cdr n))

; Helpers
(define (push-temp cg) (e-push-x0 (cg-asm cg)) (cg-set-depth! cg (+ (cg-depth cg) 16)))
(define (pop-to-x0 cg) (e-pop-x0 (cg-asm cg)) (cg-set-depth! cg (- (cg-depth cg) 16)))
(define (pop-to-x1 cg) (e-pop-x1 (cg-asm cg)) (cg-set-depth! cg (- (cg-depth cg) 16)))

(define (lookup-alist key alist)
  (if (null? alist) (list)
      (if (string=? key (car (car alist))) (cdr (car alist))
          (lookup-alist key (cdr alist)))))

(define (lookup-func cg name) (lookup-alist name (cg-funcs cg)))
(define (lookup-local cg name) (lookup-alist name (cg-locals cg)))

; Generate expression
(define (gen-expr cg node)
  (if (ast-number? node) (e-movz (cg-asm cg) X0 node)
      (if (ast-symbol? node) (gen-sym cg node)
          (if (ast-list? node) (gen-list cg node)
              (e-movz (cg-asm cg) X0 0)))))

(define (gen-sym cg node)
  (let ((name (ast-sym-name node)))
    (let ((local (lookup-local cg name)))
      (if (not (null? local))
          (e-ldr (cg-asm cg) X0 SP (+ local (cg-depth cg)))
          (e-movz (cg-asm cg) X0 0)))))

(define (gen-list cg node)
  (if (null? node) (e-movz (cg-asm cg) X0 0)
      (let ((first (car node)))
        (if (ast-symbol? first)
            (let ((op (ast-sym-name first)) (args (cdr node)))
              (gen-op cg op args))
            (e-movz (cg-asm cg) X0 0)))))

(define (gen-op cg op args)
  (if (string=? op "+") (gen-arith cg args 0)
      (if (string=? op "-") (gen-arith cg args 1)
          (if (string=? op "*") (gen-arith cg args 2)
              (if (string=? op "/") (gen-arith cg args 3)
                  (if (string=? op "if") (gen-if cg args)
                      (if (string=? op "let") (gen-let cg args)
                          (if (string=? op "begin") (gen-begin cg args)
                              (if (string=? op "define") 0
                                  (gen-call cg op args))))))))))

(define (gen-arith cg args operation)
  (if (null? args) (e-movz (cg-asm cg) X0 0)
      (if (null? (cdr args))
          (gen-expr cg (car args))
          (begin
            (gen-expr cg (car args))
            (push-temp cg)
            (gen-arith-rest cg (cdr args) operation)))))

(define (gen-arith-rest cg args operation)
  (gen-expr cg (car args))
  (pop-to-x1 cg)
  (if (= operation 0) (e-add (cg-asm cg) X0 X1 X0)
      (if (= operation 1) (e-sub (cg-asm cg) X0 X1 X0)
          (if (= operation 2) (e-mul (cg-asm cg) X0 X1 X0)
              (emit32 (cg-asm cg) (bitor SDIV-BASE (bitor (lsl X0 16) (bitor (lsl X1 5) X0)))))))
  (if (not (null? (cdr args)))
      (begin (push-temp cg) (gen-arith-rest cg (cdr args) operation))
      0))

(define (gen-if cg args)
  (let ((a (cg-asm cg))
        (else-lbl (new-label (cg-asm cg) "else"))
        (end-lbl (new-label (cg-asm cg) "end")))
    (gen-expr cg (car args))
    (emit32 a (bitor SUBS-IMM-BASE (lsl X0 5))) ; cmp x0, #0
    (e-bcond a COND-EQ else-lbl)
    (gen-expr cg (car (cdr args)))
    (e-b a end-lbl)
    (def-label a else-lbl)
    (if (not (null? (cdr (cdr args))))
        (gen-expr cg (car (cdr (cdr args))))
        (e-movz a X0 0))
    (def-label a end-lbl)))

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

(define (gen-call cg name args)
  (let ((func (lookup-func cg name)))
    (if (null? func) (e-movz (cg-asm cg) X0 0)
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

; Collect function definitions
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
      (e-epilogue a)
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
                (gen-toplevel cg (cdr ast))  ; skip define
                (begin (gen-expr cg node) (gen-toplevel cg (cdr ast))))
            (begin (gen-expr cg node) (gen-toplevel cg (cdr ast)))))))

; ===== Main =====

; Source to compile: (define (square x) (* x x)) (square 6) -> 36
(define source "(define (square x) (* x x)) (square 6)")

; Parse it
(define tokens (tokenize source))
(define ast (parse tokens))

; Compile it
(define cg (make-cg))
(compile-prog cg ast)

; Output bytes
(define a (cg-asm cg))
(define len (asm-pos a))
(print len)
(define (out-bytes buf idx max)
  (if (>= idx max) 0
      (begin (print (vector-ref buf idx)) (out-bytes buf (+ idx 1) max))))
(out-bytes (asm-buf a) 0 len)
