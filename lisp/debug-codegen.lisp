; Debug: Check code generation step by step

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

; Simple test: create asm and emit one instruction
(define buf (make-vector 100))

; Use a vector for state since set! doesn't work
(define state (make-vector 2))
(vector-set! state 0 buf)  ; buffer
(vector-set! state 1 0)    ; position

(define (emit-byte2 byte)
  (let ((b (vector-ref state 0))
        (p (vector-ref state 1)))
    (vector-set! b p (bitand byte 255))
    (vector-set! state 1 (+ p 1))))

(define (emit32 instr)
  (emit-byte2 (bitand instr 255))
  (emit-byte2 (bitand (asr instr 8) 255))
  (emit-byte2 (bitand (asr instr 16) 255))
  (emit-byte2 (bitand (asr instr 24) 255)))

; Test: emit MOVZ X0, #42
(define instr (bitor MOVZ-BASE (bitor (lsl 42 5) 0)))
(print instr)  ; Should be 3531604512

(emit32 instr)

; Check position
(print (vector-ref state 1))  ; Should be 4

; Check bytes
(print (vector-ref buf 0))  ; low byte
(print (vector-ref buf 1))
(print (vector-ref buf 2))
(print (vector-ref buf 3))  ; high byte
