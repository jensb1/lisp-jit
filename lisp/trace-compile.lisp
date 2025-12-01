; Trace compilation to find where bytes go missing

; ARM64 opcodes
(define MOVZ-BASE 3531603968)
(define ORR-BASE 2852127712)
(define ADD-REG-BASE 2332033024)
(define STP-PRE-BASE 2843738112)
(define LDP-POST-BASE 2831155200)
(define B-BASE 335544320)
(define RET-INSTR 3596551104)
(define MASK-7BIT 127)
(define MASK-26BIT 67108863)
(define X0 0)
(define X1 1)
(define X29 29)
(define X30 30)
(define SP 31)

; Minimal assembler
(define (make-asm)
  (let ((state (make-vector 5)))
    (vector-set! state 0 (make-vector 1024))
    (vector-set! state 1 0)
    (vector-set! state 2 (list))
    (vector-set! state 3 (list))
    (vector-set! state 4 0)
    state))

(define (asm-buffer asm) (vector-ref asm 0))
(define (asm-pos asm) (vector-ref asm 1))
(define (asm-labels asm) (vector-ref asm 2))
(define (asm-fixups asm) (vector-ref asm 3))
(define (asm-set-pos! asm pos) (vector-set! asm 1 pos))
(define (asm-set-labels! asm labels) (vector-set! asm 2 labels))
(define (asm-set-fixups! asm fixups) (vector-set! asm 3 fixups))

(define (emit-byte asm byte)
  (let ((buf (asm-buffer asm))
        (pos (asm-pos asm)))
    (vector-set! buf pos (bitand byte 255))
    (asm-set-pos! asm (+ pos 1))))

(define (emit32 asm instr)
  (emit-byte asm (bitand instr 255))
  (emit-byte asm (bitand (asr instr 8) 255))
  (emit-byte asm (bitand (asr instr 16) 255))
  (emit-byte asm (bitand (asr instr 24) 255)))

(define (emit-movz asm rd imm)
  (emit32 asm (bitor MOVZ-BASE (bitor (lsl imm 5) rd))))

(define (emit-mov-reg asm rd rm)
  (emit32 asm (bitor ORR-BASE (bitor (lsl rm 16) rd))))

(define (emit-add-reg asm rd rn rm)
  (emit32 asm (bitor ADD-REG-BASE (bitor (lsl rm 16) (bitor (lsl rn 5) rd)))))

(define (emit-stp-pre asm rt1 rt2 rn imm)
  (let ((scaled (bitand (asr imm 3) MASK-7BIT)))
    (emit32 asm (bitor STP-PRE-BASE (bitor (lsl scaled 15) (bitor (lsl rt2 10) (bitor (lsl rn 5) rt1)))))))

(define (emit-ldp-post asm rt1 rt2 rn imm)
  (let ((scaled (bitand (asr imm 3) MASK-7BIT)))
    (emit32 asm (bitor LDP-POST-BASE (bitor (lsl scaled 15) (bitor (lsl rt2 10) (bitor (lsl rn 5) rt1)))))))

(define (emit-ret asm) (emit32 asm RET-INSTR))

(define (define-label asm name)
  (asm-set-labels! asm (cons (cons name (asm-pos asm)) (asm-labels asm))))

(define (add-fixup asm label type)
  (asm-set-fixups! asm (cons (list (asm-pos asm) label type) (asm-fixups asm))))

(define (emit-b asm label) (add-fixup asm label 0) (emit32 asm B-BASE))

; Create assembler
(define asm (make-asm))

; Check buffer exists
(print 100)  ; marker
(print (vector-length (asm-buffer asm)))  ; should be 1024

; Emit prologue
(emit-stp-pre asm X29 X30 SP -16)
(print (asm-pos asm))  ; should be 4

(emit-mov-reg asm X29 SP)
(print (asm-pos asm))  ; should be 8

; Emit mov X0, #42
(emit-movz asm X0 42)
(print (asm-pos asm))  ; should be 12

; Emit epilogue
(emit-ldp-post asm X29 X30 SP 16)
(print (asm-pos asm))  ; should be 16

(emit-ret asm)
(print (asm-pos asm))  ; should be 20

; Print first few bytes
(print 200)  ; marker
(define buf (asm-buffer asm))
(print (vector-ref buf 0))
(print (vector-ref buf 1))
(print (vector-ref buf 2))
(print (vector-ref buf 3))
(print (vector-ref buf 4))
(print (vector-ref buf 5))
(print (vector-ref buf 6))
(print (vector-ref buf 7))
