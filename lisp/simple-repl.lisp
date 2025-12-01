; Simplified REPL for self-hosting compiler
; Uses only primitives available in the self-hosting compiler

; ===== I/O Helpers =====
(define (print-char ch) (write-byte 1 ch))
(define (print-newline) (print-char 10))

(define (print-str-h str idx)
  (let ((ch (string-ref str idx)))
    (if (= ch 0) 0
        (begin (print-char ch) (print-str-h str (+ idx 1))))))

(define (print-str str) (print-str-h str 0))

; Print a number (positive only for simplicity)
(define (print-num n)
  (if (< n 0)
      (begin (print-char 45) (print-num-pos (- 0 n)))
      (print-num-pos n)))

(define (print-num-pos n)
  (if (< n 10)
      (print-char (+ n 48))
      (begin
        (print-num-pos (/ n 10))
        (print-char (+ (mod n 10) 48)))))

(define (mod a b) (- a (* (/ a b) b)))

; ===== State Management =====
; state[0] = heap vector
; state[1] = heap pointer
; state[2] = input buffer
; state[3] = input length
; state[4] = input position

(define (make-state)
  (let ((s (make-vector 5)))
    (vector-set! s 0 (make-vector 4096))  ; heap
    (vector-set! s 1 0)                    ; heap ptr
    (vector-set! s 2 (make-vector 128))    ; input buffer
    (vector-set! s 3 0)                    ; input length
    (vector-set! s 4 0)                    ; input position
    s))

(define (heap s) (vector-ref s 0))
(define (heap-ptr s) (vector-ref s 1))
(define (input-buf s) (vector-ref s 2))
(define (input-len s) (vector-ref s 3))
(define (input-pos s) (vector-ref s 4))

(define (set-heap-ptr! s v) (vector-set! s 1 v))
(define (set-input-len! s v) (vector-set! s 3 v))
(define (set-input-pos! s v) (vector-set! s 4 v))

; ===== Memory (Pairs) =====
(define PAIR-TAG 1073741824)  ; 0x40000000

(define (alloc s n)
  (let ((ptr (heap-ptr s)))
    (set-heap-ptr! s (+ ptr n))
    ptr))

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

; ===== Input =====
(define (read-line s)
  (set-input-pos! s 0)
  (set-input-len! s (read-line-h s 0)))

(define (read-line-h s idx)
  (let ((ch (read-byte 0)))
    (if (or (= ch -1) (= ch 10))
        idx
        (begin
          (vector-set! (input-buf s) idx ch)
          (read-line-h s (+ idx 1))))))

; ===== Lexer =====
(define (at-end? s) (>= (input-pos s) (input-len s)))

(define (peek-char s)
  (if (at-end? s) 0
      (vector-ref (input-buf s) (input-pos s))))

(define (advance-char s)
  (set-input-pos! s (+ (input-pos s) 1)))

(define (is-ws? c) (or (= c 32) (= c 9)))
(define (is-digit? c) (and (>= c 48) (<= c 57)))

(define (skip-ws s)
  (if (at-end? s) 0
      (if (is-ws? (peek-char s))
          (begin (advance-char s) (skip-ws s))
          0)))

(define (read-num s) (read-num-h s 0))

(define (read-num-h s acc)
  (if (at-end? s) acc
      (let ((ch (peek-char s)))
        (if (is-digit? ch)
            (begin (advance-char s) (read-num-h s (+ (* acc 10) (- ch 48))))
            acc))))

; ===== Parser (minimal - just numbers and basic ops) =====
(define (parse-expr s)
  (skip-ws s)
  (if (at-end? s) 0
      (let ((ch (peek-char s)))
        (if (= ch 40)  ; '('
            (begin (advance-char s) (parse-list s))
            (if (is-digit? ch)
                (read-num s)
                (if (= ch 45)  ; '-' for negative
                    (begin (advance-char s)
                           (if (is-digit? (peek-char s))
                               (- 0 (read-num s))
                               0))
                    (read-op s)))))))

(define (parse-list s)
  (skip-ws s)
  (if (at-end? s) 0
      (let ((ch (peek-char s)))
        (if (= ch 41)  ; ')'
            (begin (advance-char s) 0)
            (let ((first (parse-expr s)))
              (mk-pair s first (parse-list s)))))))

; Read operator (returns tag: 1=+, 2=-, 3=*, 4=/)
(define (read-op s)
  (let ((ch (peek-char s)))
    (advance-char s)
    (if (= ch 43) 1      ; +
        (if (= ch 45) 2  ; -
            (if (= ch 42) 3  ; *
                (if (= ch 47) 4  ; /
                    0))))))

; ===== Evaluator =====
(define (eval-expr s expr)
  (if (is-nil? expr) 0
      (if (is-pair? expr)
          (eval-list s expr)
          expr)))  ; number

(define (eval-list s expr)
  (let ((op (get-car s expr))
        (args (get-cdr s expr)))
    (if (= op 1) (eval-add s args)       ; +
        (if (= op 2) (eval-sub s args)   ; -
            (if (= op 3) (eval-mul s args)   ; *
                (if (= op 4) (eval-div s args)   ; /
                    0))))))

(define (eval-add s args)
  (if (is-nil? args) 0
      (+ (eval-expr s (get-car s args))
         (eval-add s (get-cdr s args)))))

(define (eval-sub s args)
  (let ((first (eval-expr s (get-car s args))))
    (if (is-nil? (get-cdr s args))
        (- 0 first)
        (- first (eval-add s (get-cdr s args))))))

(define (eval-mul s args)
  (if (is-nil? args) 1
      (* (eval-expr s (get-car s args))
         (eval-mul s (get-cdr s args)))))

(define (eval-div s args)
  (/ (eval-expr s (get-car s args))
     (eval-expr s (get-car s (get-cdr s args)))))

; ===== REPL =====
(define (repl s)
  (print-str "Mini Lisp REPL\n")
  (print-str "Operators: + - * /\n")
  (print-str "Example: (+ 1 2 3)\n")
  (print-str "Empty line to quit\n\n")
  (repl-loop s))

(define (repl-loop s)
  (print-str "> ")
  (read-line s)
  (if (= (input-len s) 0)
      (begin (print-str "Bye!\n") 0)
      (begin
        (let ((expr (parse-expr s)))
          (let ((result (eval-expr s expr)))
            (print-num result)
            (print-newline)))
        (repl-loop s))))

; ===== Main =====
(define state (make-state))
(repl state)
(exit 0)
