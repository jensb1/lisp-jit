; Test: Compile a simple expression and output bytes

; Include bootstrap compiler
; (stripped down version for testing)

; Register constants
(define X0 0)
(define X1 1)
(define X9 9)
(define X29 29)
(define X30 30)
(define SP 31)
(define XZR 31)

; Condition codes
(define COND-EQ 0)
(define COND-NE 1)
(define COND-GE 10)

; ARM64 opcodes
(define MOVZ-BASE 3531603968)
(define MOVK-BASE 4068474880)
(define ORR-BASE 2852127712)
(define ADD-REG-BASE 2332033024)
(define SUB-IMM-BASE 3506438144)
(define ADD-IMM-BASE 2432696320)
(define UDIV-BASE 2596276224)
(define MSUB-BASE 2600501248)
(define SUBS-IMM-BASE 4043309087)
(define CSINC-BASE 2594113504)
(define STP-PRE-BASE 2843738112)
(define LDP-POST-BASE 2831155200)
(define STRB-BASE 956301312)
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

; Assembler
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
  (vector-set! (asm-buffer asm) (asm-pos asm) (bitand byte 255))
  (asm-set-pos! asm (+ (asm-pos asm) 1)))

(define (emit32 asm instr)
  (emit-byte asm (bitand instr 255))
  (emit-byte asm (bitand (asr instr 8) 255))
  (emit-byte asm (bitand (asr instr 16) 255))
  (emit-byte asm (bitand (asr instr 24) 255)))

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

(define (emit-mov-imm asm rd imm)
  (if (and (>= imm 0) (< imm 65536))
      (emit-movz asm rd imm)
      (emit-movz asm rd (bitand imm MASK-16BIT))))

(define (emit-mov-reg asm rd rm)
  (emit32 asm (bitor ORR-BASE (bitor (lsl rm 16) rd))))

(define (emit-add-reg asm rd rn rm)
  (emit32 asm (bitor ADD-REG-BASE (bitor (lsl rm 16) (bitor (lsl rn 5) rd)))))

(define (emit-add-imm asm rd rn imm)
  (emit32 asm (bitor ADD-IMM-BASE (bitor (lsl (bitand imm MASK-12BIT) 10) (bitor (lsl rn 5) rd)))))

(define (emit-sub-imm asm rd rn imm)
  (emit32 asm (bitor SUB-IMM-BASE (bitor (lsl (bitand imm MASK-12BIT) 10) (bitor (lsl rn 5) rd)))))

(define (emit-stp-pre asm rt1 rt2 rn imm)
  (let ((scaled (bitand (asr imm 3) MASK-7BIT)))
    (emit32 asm (bitor STP-PRE-BASE (bitor (lsl scaled 15) (bitor (lsl rt2 10) (bitor (lsl rn 5) rt1)))))))

(define (emit-ldp-post asm rt1 rt2 rn imm)
  (let ((scaled (bitand (asr imm 3) MASK-7BIT)))
    (emit32 asm (bitor LDP-POST-BASE (bitor (lsl scaled 15) (bitor (lsl rt2 10) (bitor (lsl rn 5) rt1)))))))

(define (emit-b asm label) (add-fixup asm label 0) (emit32 asm B-BASE))
(define (emit-ret asm) (emit32 asm RET-INSTR))

(define (emit-prologue asm) (emit-stp-pre asm X29 X30 SP -16) (emit-mov-reg asm X29 SP))
(define (emit-epilogue asm) (emit-ldp-post asm X29 X30 SP 16) (emit-ret asm))

; Fixups
(define (resolve-fixups asm)
  (resolve-fixups-h asm (asm-fixups asm)))

(define (resolve-fixups-h asm fixups)
  (if (null? fixups) 0
      (begin (resolve-one-fixup asm (car fixups)) (resolve-fixups-h asm (cdr fixups)))))

(define (resolve-one-fixup asm fixup)
  (let ((offset (car fixup))
        (label (car (cdr fixup)))
        (buf (asm-buffer asm)))
    (let ((target (lookup-label asm label)))
      (let ((ioff (asr (- target offset) 2)))
        (let ((instr (bitor (vector-ref buf offset)
                           (bitor (lsl (vector-ref buf (+ offset 1)) 8)
                                 (bitor (lsl (vector-ref buf (+ offset 2)) 16)
                                       (lsl (vector-ref buf (+ offset 3)) 24))))))
          (let ((patched (bitor instr (bitand ioff MASK-26BIT))))
            (vector-set! buf offset (bitand patched 255))
            (vector-set! buf (+ offset 1) (bitand (asr patched 8) 255))
            (vector-set! buf (+ offset 2) (bitand (asr patched 16) 255))
            (vector-set! buf (+ offset 3) (bitand (asr patched 24) 255))))))))

; ===== Generate a program that returns (+ 10 32) = 42 =====

(define asm (make-asm))

; B _main
(emit-b asm "_main")

; _main:
(define-label asm "_main")
(emit-prologue asm)

; mov X0, #10
(emit-mov-imm asm X0 10)
; mov X1, #32
(emit-mov-imm asm X1 32)
; add X0, X0, X1
(emit-add-reg asm X0 X0 X1)

(emit-epilogue asm)

; Resolve fixups
(resolve-fixups asm)

; Output
(define len (asm-pos asm))
(print len)

(define (output-bytes-h buf idx max)
  (if (>= idx max) 0
      (begin
        (print (vector-ref buf idx))
        (output-bytes-h buf (+ idx 1) max))))

(output-bytes-h (asm-buffer asm) 0 len)
