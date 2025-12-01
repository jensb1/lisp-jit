; Debug the assembler output

(define X0 0) (define X29 29) (define X30 30) (define SP 31)
(define MOVZ-BASE 3531603968)
(define ORR-BASE 2852127712)
(define STP-PRE-BASE 2843738112)
(define LDP-POST-BASE 2831155200)
(define B-BASE 335544320)
(define RET-INSTR 3596551104)
(define SVC-BASE 3556769792)
(define MASK-7BIT 127)
(define MASK-26BIT 67108863)

(define (make-asm)
  (let ((s (make-vector 5)))
    (vector-set! s 0 (make-vector 1000))
    (vector-set! s 1 0)
    (vector-set! s 2 (list))
    (vector-set! s 3 (list))
    (vector-set! s 4 0)
    s))

(define (asm-buf a) (vector-ref a 0))
(define (asm-pos a) (vector-ref a 1))
(define (asm-set-pos! a p) (vector-set! a 1 p))

(define (emit-byte a b)
  (vector-set! (asm-buf a) (asm-pos a) (bitand b 255))
  (asm-set-pos! a (+ (asm-pos a) 1)))

(define (emit32 a i)
  (emit-byte a (bitand i 255))
  (emit-byte a (bitand (asr i 8) 255))
  (emit-byte a (bitand (asr i 16) 255))
  (emit-byte a (bitand (asr i 24) 255)))

(define (e-movz a rd imm) (emit32 a (bitor MOVZ-BASE (bitor (lsl (bitand imm 65535) 5) rd))))
(define (e-mov a rd rm) (emit32 a (bitor ORR-BASE (bitor (lsl rm 16) rd))))
(define (e-stp-pre a r1 r2 rn imm) (let ((sc (bitand (asr imm 3) MASK-7BIT))) (emit32 a (bitor STP-PRE-BASE (bitor (lsl sc 15) (bitor (lsl r2 10) (bitor (lsl rn 5) r1)))))))
(define (e-ldp-post a r1 r2 rn imm) (let ((sc (bitand (asr imm 3) MASK-7BIT))) (emit32 a (bitor LDP-POST-BASE (bitor (lsl sc 15) (bitor (lsl r2 10) (bitor (lsl rn 5) r1)))))))
(define (e-ret a) (emit32 a RET-INSTR))
(define (e-svc a imm) (emit32 a (bitor SVC-BASE (lsl imm 5))))
(define (e-prologue a) (e-stp-pre a X29 X30 SP -16) (e-mov a X29 SP))
(define (e-epilogue a) (e-ldp-post a X29 X30 SP 16) (e-ret a))

; Test
(define a (make-asm))

(print 111)
(print (asm-pos a))

(e-prologue a)
(print (asm-pos a))  ; Should be 8

(e-movz a X0 120)    ; Return 120
(print (asm-pos a))  ; Should be 12

(e-movz a 16 1)      ; X16 = SYS_EXIT
(print (asm-pos a))  ; Should be 16

(e-svc a 128)        ; syscall
(print (asm-pos a))  ; Should be 20

; Print first bytes
(print 222)
(define buf (asm-buf a))
(print (vector-ref buf 0))
(print (vector-ref buf 1))
(print (vector-ref buf 2))
(print (vector-ref buf 3))
(print (vector-ref buf 4))
(print (vector-ref buf 5))
(print (vector-ref buf 6))
(print (vector-ref buf 7))
