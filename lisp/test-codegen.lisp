; Test the code generator by generating code and outputting bytes

; ===== Include required codegen.lisp code (stripped down) =====

; Register constants
(define X0 0)
(define X1 1)
(define X9 9)
(define X29 29)
(define X30 30)
(define SP 31)

; Condition codes
(define COND-EQ 0)

; ARM64 instruction base opcodes (decimal)
(define MOVZ-BASE 3531603968)
(define ORR-BASE 2852127712)
(define ADD-IMM-BASE 2432696320)
(define SUB-IMM-BASE 3506438144)
(define ADD-REG-BASE 2332033024)
(define STP-PRE-BASE 2843738112)
(define LDP-POST-BASE 2831155200)
(define B-BASE 335544320)
(define RET-INSTR 3596551104)

; Bit masks
(define MASK-12BIT 4095)
(define MASK-7BIT 127)
(define MASK-26BIT 67108863)

; Assembler state
(define (make-asm)
  (let ((state (make-vector 5)))
    (vector-set! state 0 (make-vector 4096))
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

(define (emit32 asm instruction)
  (emit-byte asm (bitand instruction 255))
  (emit-byte asm (bitand (asr instruction 8) 255))
  (emit-byte asm (bitand (asr instruction 16) 255))
  (emit-byte asm (bitand (asr instruction 24) 255)))

(define (define-label asm name)
  (asm-set-labels! asm (cons (cons name (asm-pos asm)) (asm-labels asm))))

(define (add-fixup asm label type)
  (asm-set-fixups! asm (cons (list (asm-pos asm) label type) (asm-fixups asm))))

(define (lookup-label asm name)
  (lookup-label-h (asm-labels asm) name))

(define (lookup-label-h labels name)
  (if (null? labels)
      0
      (if (string=? (car (car labels)) name)
          (cdr (car labels))
          (lookup-label-h (cdr labels) name))))

; Instructions
(define (emit-movz asm rd imm)
  (emit32 asm (bitor MOVZ-BASE (bitor (lsl imm 5) rd))))

(define (emit-mov-reg asm rd rm)
  (emit32 asm (bitor ORR-BASE (bitor (lsl rm 16) rd))))

(define (emit-add-imm asm rd rn imm)
  (emit32 asm (bitor ADD-IMM-BASE
                     (bitor (lsl (bitand imm MASK-12BIT) 10)
                           (bitor (lsl rn 5) rd)))))

(define (emit-add-reg asm rd rn rm)
  (emit32 asm (bitor ADD-REG-BASE
                     (bitor (lsl rm 16)
                           (bitor (lsl rn 5) rd)))))

(define (emit-stp-pre asm rt1 rt2 rn imm)
  (let ((scaled (bitand (asr imm 3) MASK-7BIT)))
    (emit32 asm (bitor STP-PRE-BASE
                       (bitor (lsl scaled 15)
                             (bitor (lsl rt2 10)
                                   (bitor (lsl rn 5) rt1)))))))

(define (emit-ldp-post asm rt1 rt2 rn imm)
  (let ((scaled (bitand (asr imm 3) MASK-7BIT)))
    (emit32 asm (bitor LDP-POST-BASE
                       (bitor (lsl scaled 15)
                             (bitor (lsl rt2 10)
                                   (bitor (lsl rn 5) rt1)))))))

(define (emit-b asm label)
  (add-fixup asm label 0)
  (emit32 asm B-BASE))

(define (emit-ret asm)
  (emit32 asm RET-INSTR))

(define (emit-prologue asm)
  (emit-stp-pre asm X29 X30 SP -16)
  (emit-mov-reg asm X29 SP))

(define (emit-epilogue asm)
  (emit-ldp-post asm X29 X30 SP 16)
  (emit-ret asm))

; Resolve fixups
(define (resolve-fixups asm)
  (resolve-fixups-h asm (asm-fixups asm)))

(define (resolve-fixups-h asm fixups)
  (if (null? fixups)
      0
      (begin
        (resolve-one-fixup asm (car fixups))
        (resolve-fixups-h asm (cdr fixups)))))

(define (resolve-one-fixup asm fixup)
  (let ((offset (car fixup))
        (label (car (cdr fixup)))
        (type (car (cdr (cdr fixup)))))
    (let ((target (lookup-label asm label))
          (buf (asm-buffer asm)))
      (let ((instr-offset (asr (- target offset) 2)))
        ; Read instruction
        (let ((instruction (bitor (vector-ref buf offset)
                                 (bitor (lsl (vector-ref buf (+ offset 1)) 8)
                                       (bitor (lsl (vector-ref buf (+ offset 2)) 16)
                                             (lsl (vector-ref buf (+ offset 3)) 24))))))
          ; Patch B instruction
          (let ((patched (bitor instruction (bitand instr-offset MASK-26BIT))))
            (vector-set! buf offset (bitand patched 255))
            (vector-set! buf (+ offset 1) (bitand (asr patched 8) 255))
            (vector-set! buf (+ offset 2) (bitand (asr patched 16) 255))
            (vector-set! buf (+ offset 3) (bitand (asr patched 24) 255))))))))

; ===== Generate a simple program =====
; Program: return (+ 10 32) = 42

(define asm (make-asm))

; B _main
(emit-b asm "_main")

; _main:
(define-label asm "_main")
(emit-prologue asm)

; mov X0, #10
(emit-movz asm X0 10)
; mov X1, #32
(emit-movz asm X1 32)
; add X0, X0, X1
(emit-add-reg asm X0 X0 X1)

(emit-epilogue asm)

; Resolve branch fixups
(resolve-fixups asm)

; Output the bytes as comma-separated values
(print (asm-pos asm))  ; number of bytes

; Print each byte
(define (print-bytes buf idx max)
  (if (>= idx max)
      0
      (begin
        (print (vector-ref buf idx))
        (print-bytes buf (+ idx 1) max))))

(print-bytes (asm-buffer asm) 0 (asm-pos asm))
