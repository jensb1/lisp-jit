; Self-Hosting Lisp REPL
; A Read-Eval-Print Loop running in compiled Lisp
; All mutable state is stored in vectors (no set!)

; ===== Constants =====
(define SYS-EXIT 1)
(define SYS-READ 3)
(define SYS-WRITE 4)

; ===== State Vectors =====
; Global state stored in a vector to avoid set!
; state[0] = heap vector (1MB)
; state[1] = heap pointer
; state[2] = input buffer (1KB)
; state[3] = input length
; state[4] = input position
; state[5] = environment (list of bindings)

(define (make-state)
  (let ((s (make-vector 6)))
    (vector-set! s 0 (make-vector 65536))  ; heap (64K slots)
    (vector-set! s 1 0)                     ; heap ptr
    (vector-set! s 2 (make-vector 256))     ; input buffer
    (vector-set! s 3 0)                     ; input length
    (vector-set! s 4 0)                     ; input position
    (vector-set! s 5 0)                     ; environment (nil = 0)
    s))

(define (heap s) (vector-ref s 0))
(define (heap-ptr s) (vector-ref s 1))
(define (input-buf s) (vector-ref s 2))
(define (input-len s) (vector-ref s 3))
(define (input-pos s) (vector-ref s 4))
(define (env s) (vector-ref s 5))

(define (set-heap-ptr! s v) (vector-set! s 1 v))
(define (set-input-len! s v) (vector-set! s 3 v))
(define (set-input-pos! s v) (vector-set! s 4 v))
(define (set-env! s v) (vector-set! s 5 v))

; ===== Memory Allocation =====
(define PAIR-TAG 1073741824)  ; 0x40000000

(define (alloc s n)
  (let ((ptr (heap-ptr s)))
    (set-heap-ptr! s (+ ptr n))
    ptr))

; ===== Pairs =====
(define (mk-pair s a b)
  (let ((ptr (alloc s 2)))
    (vector-set! (heap s) ptr a)
    (vector-set! (heap s) (+ ptr 1) b)
    (bitor ptr PAIR-TAG)))

(define (is-pair? x) (= (bitand x PAIR-TAG) PAIR-TAG))
(define (is-nil? x) (= x 0))

(define (get-car s p)
  (if (is-pair? p)
      (vector-ref (heap s) (bitand p (bitnot PAIR-TAG)))
      0))

(define (get-cdr s p)
  (if (is-pair? p)
      (vector-ref (heap s) (+ (bitand p (bitnot PAIR-TAG)) 1))
      0))

; ===== I/O =====
(define (write-char ch)
  (let ((buf (make-vector 1)))
    (vector-set! buf 0 ch)
    (write 1 buf 1)))

(define (write-str str)
  (write-str-h str 0))

(define (write-str-h str idx)
  (let ((ch (string-ref str idx)))
    (if (= ch 0) 0
        (begin (write-char ch) (write-str-h str (+ idx 1))))))

(define (write-num n)
  (if (< n 0)
      (begin (write-char 45) (write-num-pos (- 0 n)))
      (write-num-pos n)))

(define (write-num-pos n)
  (if (< n 10)
      (write-char (+ n 48))
      (begin
        (write-num-pos (/ n 10))
        (write-char (+ (mod n 10) 48)))))

(define (mod a b) (- a (* (/ a b) b)))

(define (write-newline) (write-char 10))

(define (read-char-stdin)
  (let ((buf (make-vector 1)))
    (let ((n (read 0 buf 1)))
      (if (= n 0) -1
          (vector-ref buf 0)))))

; ===== Read Line =====
(define (read-line s)
  (set-input-pos! s 0)
  (set-input-len! s (read-line-h s 0)))

(define (read-line-h s idx)
  (let ((ch (read-char-stdin)))
    (if (or (= ch -1) (= ch 10))  ; EOF or newline
        idx
        (begin
          (vector-set! (input-buf s) idx ch)
          (read-line-h s (+ idx 1))))))

; ===== Lexer =====
(define (at-end? s)
  (>= (input-pos s) (input-len s)))

(define (peek-char s)
  (if (at-end? s) 0
      (vector-ref (input-buf s) (input-pos s))))

(define (advance-char s)
  (set-input-pos! s (+ (input-pos s) 1)))

(define (is-ws? c) (or (= c 32) (= c 9) (= c 13)))
(define (is-digit? c) (and (>= c 48) (<= c 57)))
(define (is-alpha? c) (or (and (>= c 65) (<= c 90)) (and (>= c 97) (<= c 122))))
(define (is-sym-char? c)
  (or (is-alpha? c) (is-digit? c) (= c 45) (= c 43) (= c 42) (= c 47)
      (= c 60) (= c 62) (= c 61) (= c 95) (= c 33) (= c 63)))

(define (skip-ws s)
  (if (at-end? s) 0
      (if (is-ws? (peek-char s))
          (begin (advance-char s) (skip-ws s))
          0)))

(define (read-num s)
  (read-num-h s 0))

(define (read-num-h s acc)
  (if (at-end? s) acc
      (let ((ch (peek-char s)))
        (if (is-digit? ch)
            (begin (advance-char s) (read-num-h s (+ (* acc 10) (- ch 48))))
            acc))))

; Read symbol as a tagged number (simplified - just hash the chars)
(define SYMBOL-TAG 536870912)  ; 0x20000000

(define (read-sym s)
  (bitor (read-sym-hash s 0) SYMBOL-TAG))

(define (read-sym-hash s acc)
  (if (at-end? s) acc
      (let ((ch (peek-char s)))
        (if (is-sym-char? ch)
            (begin (advance-char s) (read-sym-hash s (+ (* acc 31) ch)))
            acc))))

(define (is-sym? x) (= (bitand x SYMBOL-TAG) SYMBOL-TAG))
(define (sym-hash x) (bitand x (bitnot SYMBOL-TAG)))

; Known symbol hashes (precomputed)
(define SYM-PLUS (+ (* 43 1) 0))        ; "+"
(define SYM-MINUS (+ (* 45 1) 0))       ; "-"
(define SYM-STAR (+ (* 42 1) 0))        ; "*"
(define SYM-SLASH (+ (* 47 1) 0))       ; "/"
(define SYM-EQ (+ (* 61 1) 0))          ; "="
(define SYM-LT (+ (* 60 1) 0))          ; "<"
(define SYM-GT (+ (* 62 1) 0))          ; ">"
(define SYM-IF (+ (* (+ (* 105 31) 102) 1) 0))  ; "if" = 105*31 + 102

; Compute hash for comparison
(define (sym-is? x name)
  (= (sym-hash x) (hash-str name 0)))

(define (hash-str str idx)
  (let ((ch (string-ref str idx)))
    (if (= ch 0) 0
        (+ (* (hash-str str (+ idx 1)) 31) ch))))

; ===== Parser =====
(define (parse-expr s)
  (skip-ws s)
  (if (at-end? s) 0
      (let ((ch (peek-char s)))
        (if (= ch 40)  ; '('
            (begin (advance-char s) (parse-list s))
            (if (= ch 39)  ; quote
                (begin (advance-char s)
                       (mk-pair s (bitor (hash-str "quote" 0) SYMBOL-TAG)
                                (mk-pair s (parse-expr s) 0)))
                (if (is-digit? ch)
                    (read-num s)
                    (if (= ch 45)  ; '-'
                        (begin
                          (advance-char s)
                          (if (is-digit? (peek-char s))
                              (- 0 (read-num s))
                              (begin
                                (set-input-pos! s (- (input-pos s) 1))
                                (read-sym s))))
                        (read-sym s))))))))

(define (parse-list s)
  (skip-ws s)
  (if (at-end? s) 0
      (let ((ch (peek-char s)))
        (if (= ch 41)  ; ')'
            (begin (advance-char s) 0)
            (let ((first (parse-expr s)))
              (mk-pair s first (parse-list s)))))))

; ===== Evaluator =====
(define (eval-expr s expr env)
  (if (is-nil? expr) 0
      (if (is-sym? expr)
          (env-lookup s env expr)
          (if (is-pair? expr)
              (eval-list s expr env)
              expr))))  ; number

(define (eval-list s expr env)
  (let ((op (get-car s expr))
        (args (get-cdr s expr)))
    (if (is-sym? op)
        (let ((h (sym-hash op)))
          (if (= h SYM-PLUS) (eval-add s args env)
              (if (= h SYM-MINUS) (eval-sub s args env)
                  (if (= h SYM-STAR) (eval-mul s args env)
                      (if (= h SYM-SLASH) (eval-div s args env)
                          (if (= h SYM-EQ) (eval-eq s args env)
                              (if (= h SYM-LT) (eval-lt s args env)
                                  (if (= h SYM-GT) (eval-gt s args env)
                                      (if (= h (hash-str "if" 0)) (eval-if s args env)
                                          (if (= h (hash-str "define" 0)) (eval-define s args env)
                                              (if (= h (hash-str "quote" 0)) (get-car s args)
                                                  (if (= h (hash-str "cons" 0)) (eval-cons s args env)
                                                      (if (= h (hash-str "car" 0)) (eval-car-op s args env)
                                                          (if (= h (hash-str "cdr" 0)) (eval-cdr-op s args env)
                                                              (if (= h (hash-str "list" 0)) (eval-list-op s args env)
                                                                  0)))))))))))))))
        0)))

(define (eval-add s args env)
  (if (is-nil? args) 0
      (+ (eval-expr s (get-car s args) env)
         (eval-add s (get-cdr s args) env))))

(define (eval-sub s args env)
  (let ((first (eval-expr s (get-car s args) env)))
    (if (is-nil? (get-cdr s args))
        (- 0 first)
        (- first (eval-add s (get-cdr s args) env)))))

(define (eval-mul s args env)
  (if (is-nil? args) 1
      (* (eval-expr s (get-car s args) env)
         (eval-mul s (get-cdr s args) env))))

(define (eval-div s args env)
  (/ (eval-expr s (get-car s args) env)
     (eval-expr s (get-car s (get-cdr s args)) env)))

(define (eval-eq s args env)
  (if (= (eval-expr s (get-car s args) env)
         (eval-expr s (get-car s (get-cdr s args)) env)) 1 0))

(define (eval-lt s args env)
  (if (< (eval-expr s (get-car s args) env)
         (eval-expr s (get-car s (get-cdr s args)) env)) 1 0))

(define (eval-gt s args env)
  (if (> (eval-expr s (get-car s args) env)
         (eval-expr s (get-car s (get-cdr s args)) env)) 1 0))

(define (eval-if s args env)
  (let ((cond (eval-expr s (get-car s args) env)))
    (if (not (= cond 0))
        (eval-expr s (get-car s (get-cdr s args)) env)
        (eval-expr s (get-car s (get-cdr s (get-cdr s args))) env))))

(define (eval-define s args env)
  (let ((name (get-car s args))
        (val (eval-expr s (get-car s (get-cdr s args)) env)))
    (set-env! s (mk-pair s (mk-pair s name val) (env s)))
    val))

(define (eval-cons s args env)
  (mk-pair s (eval-expr s (get-car s args) env)
           (eval-expr s (get-car s (get-cdr s args)) env)))

(define (eval-car-op s args env)
  (get-car s (eval-expr s (get-car s args) env)))

(define (eval-cdr-op s args env)
  (get-cdr s (eval-expr s (get-car s args) env)))

(define (eval-list-op s args env)
  (if (is-nil? args) 0
      (mk-pair s (eval-expr s (get-car s args) env)
               (eval-list-op s (get-cdr s args) env))))

; ===== Environment Lookup =====
(define (env-lookup s env sym)
  (if (is-nil? env) 0
      (let ((binding (get-car s env)))
        (if (= (get-car s binding) sym)
            (get-cdr s binding)
            (env-lookup s (get-cdr s env) sym)))))

; ===== Print Value =====
(define (print-val s v)
  (if (is-nil? v)
      (write-str "()")
      (if (is-pair? v)
          (print-list s v)
          (if (is-sym? v)
              (begin (write-char 39) (write-num (sym-hash v)))  ; 'hash
              (write-num v)))))

(define (print-list s lst)
  (write-char 40)
  (print-list-items s lst)
  (write-char 41))

(define (print-list-items s lst)
  (if (is-nil? lst) 0
      (begin
        (print-val s (get-car s lst))
        (if (not (is-nil? (get-cdr s lst)))
            (begin (write-char 32) (print-list-items s (get-cdr s lst)))
            0))))

; ===== REPL =====
(define (repl s)
  (write-str "Lisp REPL\n")
  (write-str "Operators: + - * / = < > if define cons car cdr list quote\n")
  (write-str "Examples: (+ 1 2), (* 3 4), (if (> 5 3) 100 200)\n")
  (write-str "Enter empty line to quit\n\n")
  (repl-loop s))

(define (repl-loop s)
  (write-str "> ")
  (read-line s)
  (if (= (input-len s) 0)
      (begin (write-str "Bye!\n") 0)
      (begin
        (let ((expr (parse-expr s)))
          (let ((result (eval-expr s expr (env s))))
            (print-val s result)
            (write-newline)))
        (repl-loop s))))

; ===== Main =====
(define state (make-state))
(repl state)
(exit 0)
