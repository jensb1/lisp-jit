; Working Bootstrap - Compile (define (double x) (* x 2)) (double 21) -> 42

; ===== Constants =====
(define X0 0) (define X1 1) (define X9 9) (define X29 29) (define X30 30) (define SP 31) (define XZR 31)
(define COND-EQ 0) (define COND-NE 1) (define COND-GE 10) (define COND-LT 11)

(define MOVZ-BASE 3531603968)
(define ORR-BASE 2852127712)
(define ADD-REG-BASE 2332033024)
(define SUB-IMM-BASE 3506438144)
(define ADD-IMM-BASE 2432696320)
(define MUL-BASE 2600500224)
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

(define MASK-12BIT 4095)
(define MASK-9BIT 511)
(define MASK-7BIT 127)
(define MASK-26BIT 67108863)
(define MASK-19BIT 524287)

; ===== Assembler =====
(define (make-asm)
  (let ((s (make-vector 5)))
    (vector-set! s 0 (make-vector 4096))
    (vector-set! s 1 0)
    (vector-set! s 2 (list))
    (vector-set! s 3 (list))
    (vector-set! s 4 0)
    s))

(define (asm-buf a) (vector-ref a 0))
(define (asm-pos a) (vector-ref a 1))
(define (asm-labels a) (vector-ref a 2))
(define (asm-fixups a) (vector-ref a 3))
(define (asm-set-pos! a p) (vector-set! a 1 p))
(define (asm-set-labels! a l) (vector-set! a 2 l))
(define (asm-set-fixups! a f) (vector-set! a 3 f))

(define (emit-byte a b)
  (vector-set! (asm-buf a) (asm-pos a) (bitand b 255))
  (asm-set-pos! a (+ (asm-pos a) 1)))

(define (emit32 a i)
  (emit-byte a (bitand i 255))
  (emit-byte a (bitand (asr i 8) 255))
  (emit-byte a (bitand (asr i 16) 255))
  (emit-byte a (bitand (asr i 24) 255)))

(define (def-label a n)
  (asm-set-labels! a (cons (cons n (asm-pos a)) (asm-labels a))))

(define (add-fixup a l t)
  (asm-set-fixups! a (cons (list (asm-pos a) l t) (asm-fixups a))))

(define (find-label ls n)
  (if (null? ls) 0
      (if (string=? (car (car ls)) n) (cdr (car ls)) (find-label (cdr ls) n))))

(define (lookup-label a n) (find-label (asm-labels a) n))

; Instructions
(define (e-movz a rd im) (emit32 a (bitor MOVZ-BASE (bitor (lsl im 5) rd))))
(define (e-mov a rd rm) (emit32 a (bitor ORR-BASE (bitor (lsl rm 16) rd))))
(define (e-add a rd rn rm) (emit32 a (bitor ADD-REG-BASE (bitor (lsl rm 16) (bitor (lsl rn 5) rd)))))
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

; ===== Generate: (define (double x) (* x 2)) (double 21) =====
(define a (make-asm))

; B _main
(e-b a "_main")

; _lisp_double:
; double(x) = x * 2
(def-label a "_lisp_double")
(e-prologue a)
(e-alloc a 16)              ; stack for 1 param
(e-str a X0 SP 8)           ; store x at SP+8
; body: (* x 2)
(e-ldr a X0 SP 8)           ; load x
(e-push-x0 a)               ; push x
(e-movz a X0 2)             ; load 2
(e-pop-x1 a)                ; pop x to X1
(e-mul a X0 X1 X0)          ; X0 = X1 * X0
(e-free a 16)
(e-epilogue a)

; _main:
(def-label a "_main")
(e-prologue a)
; call double(21)
(e-movz a X0 21)            ; arg = 21
(e-push-x0 a)               ; push arg
(e-ldr-post a X0 SP 16)     ; pop to X0
(e-bl a "_lisp_double")     ; call double
; return result
(e-epilogue a)

; Resolve fixups
(resolve-fixups a)

; Output
(define len (asm-pos a))
(print len)
(define (out-bytes buf idx max)
  (if (>= idx max) 0
      (begin (print (vector-ref buf idx)) (out-bytes buf (+ idx 1) max))))
(out-bytes (asm-buf a) 0 len)
