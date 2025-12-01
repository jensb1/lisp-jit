; Lisp Code Generator - Emits ARM64 machine code
; This generates binary code from AST (output of parser)

; ===== Constants =====

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

; ARM64 instruction base opcodes (decimal)
(define MOVZ-BASE 3531603968)      ; 0xD2800000
(define MOVN-BASE 2457862144)      ; 0x92800000
(define MOVK-BASE 4068474880)      ; 0xF2800000
(define ORR-BASE 2852127712)       ; 0xAA0003E0 (mov reg)
(define ADD-REG-BASE 2332033024)   ; 0x8B000000
(define ADD-IMM-BASE 2432696320)   ; 0x91000000
(define SUB-REG-BASE 3405774848)   ; 0xCB000000
(define SUB-IMM-BASE 3506438144)   ; 0xD1000000
(define MUL-BASE 2600500224)       ; 0x9B007C00
(define SDIV-BASE 2596277248)      ; 0x9AC00C00
(define UDIV-BASE 2596276224)      ; 0x9AC00800
(define MSUB-BASE 2600501248)      ; 0x9B008000
(define SUBS-REG-BASE 3942645791)  ; 0xEB00001F (cmp reg)
(define SUBS-IMM-BASE 4043309087)  ; 0xF100001F (cmp imm)
(define CSINC-BASE 2594113504)     ; 0x9A9F07E0 (cset)
(define LDR-IMM-BASE 4181721088)   ; 0xF9400000
(define STR-IMM-BASE 4177526784)   ; 0xF9000000
(define LDR-POST-BASE 4164944896)  ; 0xF8400400
(define STR-PRE-BASE 4160752640)   ; 0xF8000C00
(define STRB-BASE 956301312)       ; 0x39000000
(define STP-PRE-BASE 2843738112)   ; 0xA9800000
(define LDP-POST-BASE 2831155200)  ; 0xA8C00000
(define B-BASE 335544320)          ; 0x14000000
(define BL-BASE 2483027968)        ; 0x94000000
(define BLR-BASE 3594452992)       ; 0xD63F0000
(define RET-INSTR 3596551104)      ; 0xD65F03C0
(define BCOND-BASE 1409286144)     ; 0x54000000

; Bit masks
(define MASK-16BIT 65535)          ; 0xFFFF
(define MASK-12BIT 4095)           ; 0xFFF
(define MASK-9BIT 511)             ; 0x1FF
(define MASK-7BIT 127)             ; 0x7F
(define MASK-26BIT 67108863)       ; 0x3FFFFFF
(define MASK-19BIT 524287)         ; 0x7FFFF

; AST type markers (from parser)
; Numbers: raw values
; Symbols: (3 . name-string)
; Strings: (4 . value-string)
; Lists: Lisp lists of AST nodes

; ===== Assembler State =====
; State is a vector:
;   0: buffer - byte vector
;   1: buffer-pos - current write position
;   2: labels - association list ((name . offset) ...)
;   3: fixups - list of (offset label type) tuples
;   4: label-counter - for generating unique labels

(define (make-asm)
  (let ((state (make-vector 5)))
    (vector-set! state 0 (make-vector 65536))  ; 64K code buffer
    (vector-set! state 1 0)   ; buffer-pos
    (vector-set! state 2 (list))  ; labels
    (vector-set! state 3 (list))  ; fixups
    (vector-set! state 4 0)   ; label-counter
    state))

(define (asm-buffer asm) (vector-ref asm 0))
(define (asm-pos asm) (vector-ref asm 1))
(define (asm-labels asm) (vector-ref asm 2))
(define (asm-fixups asm) (vector-ref asm 3))
(define (asm-label-counter asm) (vector-ref asm 4))

(define (asm-set-pos! asm pos) (vector-set! asm 1 pos))
(define (asm-set-labels! asm labels) (vector-set! asm 2 labels))
(define (asm-set-fixups! asm fixups) (vector-set! asm 3 fixups))
(define (asm-set-label-counter! asm n) (vector-set! asm 4 n))

; ===== Byte Emission =====

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

; ===== Label Management =====

(define (new-label asm prefix)
  (let ((n (asm-label-counter asm)))
    (asm-set-label-counter! asm (+ n 1))
    (string-append prefix (number->string n))))

(define (define-label asm name)
  (let ((pos (asm-pos asm))
        (labels (asm-labels asm)))
    (asm-set-labels! asm (cons (cons name pos) labels))))

(define (add-fixup asm label type)
  (let ((pos (asm-pos asm))
        (fixups (asm-fixups asm)))
    (asm-set-fixups! asm (cons (list pos label type) fixups))))

(define (lookup-label asm name)
  (lookup-label-helper (asm-labels asm) name))

(define (lookup-label-helper labels name)
  (if (null? labels)
      0  ; error: undefined label
      (if (string=? (car (car labels)) name)
          (cdr (car labels))
          (lookup-label-helper (cdr labels) name))))

; ===== ARM64 Instruction Encoding =====

; MOV immediate (MOVZ)
(define (emit-movz asm rd imm)
  ; MOVZ Xd, #imm => 0xD2800000 | (imm << 5) | rd
  (let ((instruction (bitor MOVZ-BASE (bitor (lsl imm 5) rd))))
    (emit32 asm instruction)))

; MOVN (for negative immediates)
(define (emit-movn asm rd imm)
  ; MOVN Xd, #imm => 0x92800000 | (imm << 5) | rd
  (let ((instruction (bitor MOVN-BASE (bitor (lsl imm 5) rd))))
    (emit32 asm instruction)))

; MOVK (move keep)
(define (emit-movk asm rd imm shift)
  ; MOVK Xd, #imm, LSL #shift => 0xF2800000 | (hw << 21) | (imm << 5) | rd
  (let ((hw (asr shift 4)))
    (let ((instruction (bitor MOVK-BASE
                             (bitor (lsl hw 21)
                                   (bitor (lsl (bitand imm MASK-16BIT) 5) rd)))))
      (emit32 asm instruction))))

; Load 64-bit immediate (handles small positive values)
(define (emit-mov-imm asm rd imm)
  (if (and (>= imm 0) (< imm 65536))
      (emit-movz asm rd imm)
      (if (and (< imm 0) (> imm -65536))
          (emit-movn asm rd (bitand (bitnot imm) MASK-16BIT))
          (begin
            (emit-movz asm rd (bitand imm MASK-16BIT))
            (let ((imm16 (bitand (asr imm 16) MASK-16BIT)))
              (if (not (= imm16 0))
                  (emit-movk asm rd imm16 16)
                  0))
            (let ((imm32 (bitand (asr imm 32) MASK-16BIT)))
              (if (not (= imm32 0))
                  (emit-movk asm rd imm32 32)
                  0))
            (let ((imm48 (bitand (asr imm 48) MASK-16BIT)))
              (if (not (= imm48 0))
                  (emit-movk asm rd imm48 48)
                  0))))))

; MOV register (ORR Xd, XZR, Xm)
(define (emit-mov-reg asm rd rm)
  ; ORR Xd, XZR, Xm => 0xAA0003E0 | (rm << 16) | rd
  (let ((instruction (bitor ORR-BASE (bitor (lsl rm 16) rd))))
    (emit32 asm instruction)))

; ADD register
(define (emit-add-reg asm rd rn rm)
  ; ADD Xd, Xn, Xm => 0x8B000000 | (rm << 16) | (rn << 5) | rd
  (let ((instruction (bitor ADD-REG-BASE
                           (bitor (lsl rm 16)
                                 (bitor (lsl rn 5) rd)))))
    (emit32 asm instruction)))

; ADD immediate
(define (emit-add-imm asm rd rn imm)
  ; ADD Xd, Xn, #imm => 0x91000000 | (imm << 10) | (rn << 5) | rd
  (let ((instruction (bitor ADD-IMM-BASE
                           (bitor (lsl (bitand imm MASK-12BIT) 10)
                                 (bitor (lsl rn 5) rd)))))
    (emit32 asm instruction)))

; SUB register
(define (emit-sub-reg asm rd rn rm)
  ; SUB Xd, Xn, Xm => 0xCB000000 | (rm << 16) | (rn << 5) | rd
  (let ((instruction (bitor SUB-REG-BASE
                           (bitor (lsl rm 16)
                                 (bitor (lsl rn 5) rd)))))
    (emit32 asm instruction)))

; SUB immediate
(define (emit-sub-imm asm rd rn imm)
  ; SUB Xd, Xn, #imm => 0xD1000000 | (imm << 10) | (rn << 5) | rd
  (let ((instruction (bitor SUB-IMM-BASE
                           (bitor (lsl (bitand imm MASK-12BIT) 10)
                                 (bitor (lsl rn 5) rd)))))
    (emit32 asm instruction)))

; MUL (MADD Xd, Xn, Xm, XZR)
(define (emit-mul asm rd rn rm)
  ; MUL Xd, Xn, Xm => 0x9B007C00 | (rm << 16) | (rn << 5) | rd
  (let ((instruction (bitor MUL-BASE
                           (bitor (lsl rm 16)
                                 (bitor (lsl rn 5) rd)))))
    (emit32 asm instruction)))

; SDIV
(define (emit-sdiv asm rd rn rm)
  ; SDIV Xd, Xn, Xm => 0x9AC00C00 | (rm << 16) | (rn << 5) | rd
  (let ((instruction (bitor SDIV-BASE
                           (bitor (lsl rm 16)
                                 (bitor (lsl rn 5) rd)))))
    (emit32 asm instruction)))

; UDIV
(define (emit-udiv asm rd rn rm)
  ; UDIV Xd, Xn, Xm => 0x9AC00800 | (rm << 16) | (rn << 5) | rd
  (let ((instruction (bitor UDIV-BASE
                           (bitor (lsl rm 16)
                                 (bitor (lsl rn 5) rd)))))
    (emit32 asm instruction)))

; MSUB Xd, Xn, Xm, Xa (Xd = Xa - Xn * Xm)
(define (emit-msub asm rd rn rm ra)
  ; MSUB => 0x9B008000 | (rm << 16) | (ra << 10) | (rn << 5) | rd
  (let ((instruction (bitor MSUB-BASE
                           (bitor (lsl rm 16)
                                 (bitor (lsl ra 10)
                                       (bitor (lsl rn 5) rd))))))
    (emit32 asm instruction)))

; NEG Xd, Xm (SUB Xd, XZR, Xm)
(define (emit-neg asm rd rm)
  (emit-sub-reg asm rd XZR rm))

; CMP register (SUBS XZR, Xn, Xm)
(define (emit-cmp-reg asm rn rm)
  ; SUBS XZR, Xn, Xm => 0xEB00001F | (rm << 16) | (rn << 5)
  (let ((instruction (bitor SUBS-REG-BASE
                           (bitor (lsl rm 16) (lsl rn 5)))))
    (emit32 asm instruction)))

; CMP immediate (SUBS XZR, Xn, #imm)
(define (emit-cmp-imm asm rn imm)
  ; SUBS XZR, Xn, #imm => 0xF100001F | (imm << 10) | (rn << 5)
  (let ((instruction (bitor SUBS-IMM-BASE
                           (bitor (lsl (bitand imm MASK-12BIT) 10) (lsl rn 5)))))
    (emit32 asm instruction)))

; CSET Xd, cond (CSINC Xd, XZR, XZR, invert(cond))
(define (emit-cset asm rd cond)
  ; CSINC Xd, XZR, XZR, inv => 0x9A9F07E0 | (inv << 12) | rd
  (let ((inv (bitxor cond 1)))
    (let ((instruction (bitor CSINC-BASE (bitor (lsl inv 12) rd))))
      (emit32 asm instruction))))

; LDR Xt, [Xn, #imm]
(define (emit-ldr-imm asm rt rn imm)
  ; LDR Xt, [Xn, #imm] => 0xF9400000 | (scaled_imm << 10) | (rn << 5) | rt
  (let ((scaled (asr imm 3)))
    (let ((instruction (bitor LDR-IMM-BASE
                             (bitor (lsl (bitand scaled MASK-12BIT) 10)
                                   (bitor (lsl rn 5) rt)))))
      (emit32 asm instruction))))

; STR Xt, [Xn, #imm]
(define (emit-str-imm asm rt rn imm)
  ; STR Xt, [Xn, #imm] => 0xF9000000 | (scaled_imm << 10) | (rn << 5) | rt
  (let ((scaled (asr imm 3)))
    (let ((instruction (bitor STR-IMM-BASE
                             (bitor (lsl (bitand scaled MASK-12BIT) 10)
                                   (bitor (lsl rn 5) rt)))))
      (emit32 asm instruction))))

; LDR Xt, [Xn], #imm (post-index)
(define (emit-ldr-post asm rt rn imm)
  ; LDR Xt, [Xn], #imm => 0xF8400400 | (imm9 << 12) | (rn << 5) | rt
  (let ((instruction (bitor LDR-POST-BASE
                           (bitor (lsl (bitand imm MASK-9BIT) 12)
                                 (bitor (lsl rn 5) rt)))))
    (emit32 asm instruction)))

; STR Xt, [Xn, #imm]! (pre-index)
(define (emit-str-pre asm rt rn imm)
  ; STR Xt, [Xn, #imm]! => 0xF8000C00 | (imm9 << 12) | (rn << 5) | rt
  (let ((instruction (bitor STR-PRE-BASE
                           (bitor (lsl (bitand imm MASK-9BIT) 12)
                                 (bitor (lsl rn 5) rt)))))
    (emit32 asm instruction)))

; STRB Wt, [Xn, #imm] (store byte)
(define (emit-strb asm rt rn imm)
  ; STRB Wt, [Xn, #imm] => 0x39000000 | (imm << 10) | (rn << 5) | rt
  (let ((instruction (bitor STRB-BASE
                           (bitor (lsl (bitand imm MASK-12BIT) 10)
                                 (bitor (lsl rn 5) rt)))))
    (emit32 asm instruction)))

; STP Xt1, Xt2, [Xn, #imm]! (pre-index)
(define (emit-stp-pre asm rt1 rt2 rn imm)
  ; STP => 0xA9800000 | (imm7 << 15) | (rt2 << 10) | (rn << 5) | rt1
  (let ((scaled (bitand (asr imm 3) MASK-7BIT)))
    (let ((instruction (bitor STP-PRE-BASE
                             (bitor (lsl scaled 15)
                                   (bitor (lsl rt2 10)
                                         (bitor (lsl rn 5) rt1))))))
      (emit32 asm instruction))))

; LDP Xt1, Xt2, [Xn], #imm (post-index)
(define (emit-ldp-post asm rt1 rt2 rn imm)
  ; LDP => 0xA8C00000 | (imm7 << 15) | (rt2 << 10) | (rn << 5) | rt1
  (let ((scaled (bitand (asr imm 3) MASK-7BIT)))
    (let ((instruction (bitor LDP-POST-BASE
                             (bitor (lsl scaled 15)
                                   (bitor (lsl rt2 10)
                                         (bitor (lsl rn 5) rt1))))))
      (emit32 asm instruction))))

; B label (unconditional branch)
(define (emit-b asm label)
  (add-fixup asm label 0)  ; type 0 = B
  (emit32 asm B-BASE))

; BL label (branch with link)
(define (emit-bl asm label)
  (add-fixup asm label 1)  ; type 1 = BL
  (emit32 asm BL-BASE))

; BLR Xn (branch with link to register)
(define (emit-blr asm rn)
  ; BLR Xn => 0xD63F0000 | (rn << 5)
  (let ((instruction (bitor BLR-BASE (lsl rn 5))))
    (emit32 asm instruction)))

; RET (return)
(define (emit-ret asm)
  (emit32 asm RET-INSTR))

; B.cond label (conditional branch)
(define (emit-bcond asm cond label)
  (add-fixup asm label 2)  ; type 2 = bcond
  (emit32 asm (bitor BCOND-BASE cond)))

; ===== High-Level Assembly Helpers =====

; Push X0 to stack
(define (emit-push-x0 asm)
  (emit-str-pre asm X0 SP -16))

; Pop to X0
(define (emit-pop-x0 asm)
  (emit-ldr-post asm X0 SP 16))

; Pop to X1
(define (emit-pop-x1 asm)
  (emit-ldr-post asm X1 SP 16))

; Function prologue
(define (emit-prologue asm)
  (emit-stp-pre asm X29 X30 SP -16)
  (emit-mov-reg asm X29 SP))

; Function epilogue
(define (emit-epilogue asm)
  (emit-ldp-post asm X29 X30 SP 16)
  (emit-ret asm))

; Allocate stack space
(define (emit-alloc-stack asm size)
  (emit-sub-imm asm SP SP size))

; Free stack space
(define (emit-free-stack asm size)
  (emit-add-imm asm SP SP size))

; ===== Fixup Resolution =====

(define (resolve-fixups asm)
  (resolve-fixups-helper asm (asm-fixups asm)))

(define (resolve-fixups-helper asm fixups)
  (if (null? fixups)
      0
      (begin
        (resolve-one-fixup asm (car fixups))
        (resolve-fixups-helper asm (cdr fixups)))))

(define (resolve-one-fixup asm fixup)
  (let ((offset (car fixup))
        (label (car (cdr fixup)))
        (type (car (cdr (cdr fixup)))))
    (let ((target (lookup-label asm label)))
      (let ((instr-offset (asr (- target offset) 2)))  ; offset in instructions
        (patch-instruction asm offset type instr-offset)))))

(define (patch-instruction asm offset type instr-offset)
  (let ((buf (asm-buffer asm)))
    ; Read existing instruction
    (let ((instruction (bitor (vector-ref buf offset)
                             (bitor (lsl (vector-ref buf (+ offset 1)) 8)
                                   (bitor (lsl (vector-ref buf (+ offset 2)) 16)
                                         (lsl (vector-ref buf (+ offset 3)) 24))))))
      ; Patch based on type
      (let ((patched
             (if (= type 0)  ; B
                 (bitor instruction (bitand instr-offset MASK-26BIT))
                 (if (= type 1)  ; BL
                     (bitor instruction (bitand instr-offset MASK-26BIT))
                     (if (= type 2)  ; bcond
                         (bitor instruction (lsl (bitand instr-offset MASK-19BIT) 5))
                         instruction)))))
        ; Write back
        (vector-set! buf offset (bitand patched 255))
        (vector-set! buf (+ offset 1) (bitand (asr patched 8) 255))
        (vector-set! buf (+ offset 2) (bitand (asr patched 16) 255))
        (vector-set! buf (+ offset 3) (bitand (asr patched 24) 255))))))

; ===== Get Final Code =====

(define (get-code asm)
  (resolve-fixups asm)
  ; Return the buffer trimmed to actual size
  (let ((pos (asm-pos asm))
        (buf (asm-buffer asm)))
    (let ((result (make-vector pos)))
      (copy-bytes buf result 0 pos)
      result)))

(define (copy-bytes src dst start count)
  (copy-bytes-helper src dst start 0 count))

(define (copy-bytes-helper src dst src-idx dst-idx count)
  (if (= dst-idx count)
      0
      (begin
        (vector-set! dst dst-idx (vector-ref src src-idx))
        (copy-bytes-helper src dst (+ src-idx 1) (+ dst-idx 1) count))))

; ===== Code Generator State =====
; Codegen state is a vector:
;   0: asm - assembler state
;   1: functions - alist of (name . (params . body)) for defined functions
;   2: globals - alist of (name . index) for global variables
;   3: locals - alist of (name . offset) for local variables
;   4: temp-stack-depth - current depth of temporaries on stack
;   5: stack-size - allocated stack size for function

(define (make-codegen)
  (let ((cg (make-vector 6)))
    (vector-set! cg 0 (make-asm))
    (vector-set! cg 1 (list))  ; functions
    (vector-set! cg 2 (list))  ; globals
    (vector-set! cg 3 (list))  ; locals
    (vector-set! cg 4 0)       ; temp-stack-depth
    (vector-set! cg 5 0)       ; stack-size
    cg))

(define (cg-asm cg) (vector-ref cg 0))
(define (cg-functions cg) (vector-ref cg 1))
(define (cg-globals cg) (vector-ref cg 2))
(define (cg-locals cg) (vector-ref cg 3))
(define (cg-temp-depth cg) (vector-ref cg 4))
(define (cg-stack-size cg) (vector-ref cg 5))

(define (cg-set-functions! cg funcs) (vector-set! cg 1 funcs))
(define (cg-set-globals! cg globals) (vector-set! cg 2 globals))
(define (cg-set-locals! cg locals) (vector-set! cg 3 locals))
(define (cg-set-temp-depth! cg d) (vector-set! cg 4 d))
(define (cg-set-stack-size! cg s) (vector-set! cg 5 s))

; ===== AST Type Checks =====

(define (ast-number? node)
  (not (pair? node)))  ; Numbers are raw values

(define (ast-symbol? node)
  (and (pair? node) (= (car node) 3)))

(define (ast-string? node)
  (and (pair? node) (= (car node) 4)))

(define (ast-list? node)
  (and (pair? node) (not (= (car node) 3)) (not (= (car node) 4))))

(define (ast-symbol-name node)
  (cdr node))

(define (ast-string-value node)
  (cdr node))

; ===== Helper Functions =====

(define (push-temp cg)
  (emit-push-x0 (cg-asm cg))
  (cg-set-temp-depth! cg (+ (cg-temp-depth cg) 16)))

(define (pop-to-x0 cg)
  (emit-pop-x0 (cg-asm cg))
  (cg-set-temp-depth! cg (- (cg-temp-depth cg) 16)))

(define (pop-to-x1 cg)
  (emit-pop-x1 (cg-asm cg))
  (cg-set-temp-depth! cg (- (cg-temp-depth cg) 16)))

(define (lookup-alist key alist)
  (if (null? alist)
      (list)
      (if (string=? key (car (car alist)))
          (cdr (car alist))
          (lookup-alist key (cdr alist)))))

(define (lookup-function cg name)
  (lookup-alist name (cg-functions cg)))

(define (lookup-global cg name)
  (lookup-alist name (cg-globals cg)))

(define (lookup-local cg name)
  (lookup-alist name (cg-locals cg)))

; List operations
(define (list-length lst)
  (if (null? lst) 0 (+ 1 (list-length (cdr lst)))))

(define (list-ref-helper lst n)
  (if (= n 0)
      (car lst)
      (list-ref-helper (cdr lst) (- n 1))))

(define (list-ref lst n)
  (list-ref-helper lst n))

(define (list-tail lst n)
  (if (= n 0)
      lst
      (list-tail (cdr lst) (- n 1))))

; ===== Expression Generation =====

(define (generate-expr cg node)
  (if (ast-number? node)
      (generate-number cg node)
      (if (ast-symbol? node)
          (generate-symbol cg node)
          (if (ast-string? node)
              (generate-string cg node)
              (if (ast-list? node)
                  (generate-list cg node)
                  0)))))

(define (generate-number cg value)
  (emit-mov-imm (cg-asm cg) X0 value))

(define (generate-symbol cg node)
  (let ((name (ast-symbol-name node)))
    (let ((local-offset (lookup-local cg name)))
      (if (not (null? local-offset))
          ; Load from stack
          (let ((adjusted (+ local-offset (cg-temp-depth cg))))
            (emit-ldr-imm (cg-asm cg) X0 SP adjusted))
          ; Check globals (would need runtime calls)
          (let ((global-idx (lookup-global cg name)))
            (if (not (null? global-idx))
                (begin
                  ; vector_ref(X28, global-idx)
                  (emit-mov-reg (cg-asm cg) X0 X28)
                  (emit-mov-imm (cg-asm cg) X1 global-idx)
                  ; Would call runtime here
                  0)
                ; Undefined symbol
                0))))))

(define (generate-string cg node)
  (let ((str (ast-string-value node)))
    ; For now, strings need runtime support
    ; This would allocate and copy the string
    (emit-mov-imm (cg-asm cg) X0 0)))

(define (generate-list cg node)
  (if (null? node)
      (emit-mov-imm (cg-asm cg) X0 0)
      (let ((first (car node)))
        (if (not (ast-symbol? first))
            0  ; Error: first element must be symbol
            (let ((op-name (ast-symbol-name first))
                  (args (cdr node)))
              (generate-call-or-builtin cg op-name args))))))

(define (generate-call-or-builtin cg op args)
  ; Check for built-in operations
  (if (string=? op "+")
      (generate-arithmetic cg args 0)  ; 0 = add
      (if (string=? op "-")
          (generate-arithmetic cg args 1)  ; 1 = sub
          (if (string=? op "*")
              (generate-arithmetic cg args 2)  ; 2 = mul
              (if (string=? op "/")
                  (generate-arithmetic cg args 3)  ; 3 = div
                  (if (string=? op "<")
                      (generate-comparison cg args COND-LT)
                      (if (string=? op ">")
                          (generate-comparison cg args COND-GT)
                          (if (string=? op "=")
                              (generate-comparison cg args COND-EQ)
                              (if (string=? op "<=")
                                  (generate-comparison cg args COND-LE)
                                  (if (string=? op ">=")
                                      (generate-comparison cg args COND-GE)
                                      (if (string=? op "if")
                                          (generate-if cg args)
                                          (if (string=? op "begin")
                                              (generate-begin cg args)
                                              (if (string=? op "let")
                                                  (generate-let cg args)
                                                  (if (string=? op "print")
                                                      (generate-print cg args)
                                                      ; Try user function
                                                      (generate-user-call cg op args)))))))))))))))

(define (generate-arithmetic cg args operation)
  (if (null? args)
      (emit-mov-imm (cg-asm cg) X0 0)
      (if (null? (cdr args))
          (begin
            (generate-expr cg (car args))
            (if (= operation 1)  ; negate for unary -
                (emit-neg (cg-asm cg) X0 X0)
                0))
          (begin
            (generate-expr cg (car args))
            (push-temp cg)
            (generate-arithmetic-rest cg (cdr args) operation)))))

(define (generate-arithmetic-rest cg args operation)
  (generate-expr cg (car args))
  (pop-to-x1 cg)
  ; X1 has left operand, X0 has right operand
  (if (= operation 0)
      (emit-add-reg (cg-asm cg) X0 X1 X0)
      (if (= operation 1)
          (emit-sub-reg (cg-asm cg) X0 X1 X0)
          (if (= operation 2)
              (emit-mul (cg-asm cg) X0 X1 X0)
              (if (= operation 3)
                  (emit-sdiv (cg-asm cg) X0 X1 X0)
                  0))))
  (if (not (null? (cdr args)))
      (begin
        (push-temp cg)
        (generate-arithmetic-rest cg (cdr args) operation))
      0))

(define (generate-comparison cg args cond)
  (generate-expr cg (car args))
  (push-temp cg)
  (generate-expr cg (car (cdr args)))
  (pop-to-x1 cg)
  (emit-cmp-reg (cg-asm cg) X1 X0)
  (emit-cset (cg-asm cg) X0 cond))

(define (generate-if cg args)
  (let ((asm (cg-asm cg))
        (else-label (new-label asm "else"))
        (end-label (new-label asm "endif")))
    ; Generate condition
    (generate-expr cg (car args))
    (emit-cmp-imm asm X0 0)
    (emit-bcond asm COND-EQ else-label)
    ; Generate then branch
    (generate-expr cg (car (cdr args)))
    (emit-b asm end-label)
    ; Generate else branch
    (define-label asm else-label)
    (if (not (null? (cdr (cdr args))))
        (generate-expr cg (car (cdr (cdr args))))
        (emit-mov-imm asm X0 0))
    (define-label asm end-label)))

(define (generate-begin cg args)
  (generate-begin-helper cg args))

(define (generate-begin-helper cg args)
  (if (null? args)
      0
      (begin
        (generate-expr cg (car args))
        (generate-begin-helper cg (cdr args)))))

(define (generate-let cg args)
  (let ((bindings (car args))
        (body (cdr args)))
    (let ((num-bindings (list-length bindings))
          (saved-locals (cg-locals cg))
          (saved-temp-depth (cg-temp-depth cg)))
      (if (> num-bindings 0)
          (let ((binding-stack-size (* (+ (/ num-bindings 2) 1) 16)))
            (emit-alloc-stack (cg-asm cg) binding-stack-size)
            (cg-set-temp-depth! cg (+ (cg-temp-depth cg) binding-stack-size))
            (generate-let-bindings cg bindings 0 binding-stack-size)
            (generate-begin cg body)
            (emit-free-stack (cg-asm cg) binding-stack-size)
            (cg-set-temp-depth! cg saved-temp-depth))
          (generate-begin cg body))
      (cg-set-locals! cg saved-locals))))

(define (generate-let-bindings cg bindings idx stack-size)
  (if (null? bindings)
      0
      (let ((binding (car bindings)))
        (let ((var-name (ast-symbol-name (car binding)))
              (var-val (car (cdr binding))))
          (generate-expr cg var-val)
          (let ((sp-offset (- stack-size (* (+ idx 1) 8))))
            (emit-str-imm (cg-asm cg) X0 SP sp-offset)
            (cg-set-locals! cg (cons (cons var-name (- sp-offset (cg-temp-depth cg)))
                                     (cg-locals cg))))
          (generate-let-bindings cg (cdr bindings) (+ idx 1) stack-size)))))

(define (generate-print cg args)
  (generate-expr cg (car args))
  (emit-bl (cg-asm cg) "_print_int"))

(define (generate-user-call cg name args)
  (let ((func (lookup-function cg name)))
    (if (null? func)
        0  ; Undefined function
        (let ((num-args (list-length args)))
          ; Push arguments in reverse order
          (generate-push-args cg args)
          ; Pop to registers
          (generate-pop-args cg num-args 0)
          ; Call function
          (emit-bl (cg-asm cg) (string-append "_lisp_" name))))))

(define (generate-push-args cg args)
  (if (null? args)
      0
      (begin
        (generate-push-args cg (cdr args))  ; Push in reverse
        (generate-expr cg (car args))
        (push-temp cg))))

(define (generate-pop-args cg num idx)
  (if (>= idx num)
      0
      (begin
        (emit-ldr-post (cg-asm cg) idx SP 16)
        (cg-set-temp-depth! cg (- (cg-temp-depth cg) 16))
        (generate-pop-args cg num (+ idx 1)))))

; ===== Print Integer Helper =====
; Same as in TypeScript codegen

(define (generate-print-int-helper cg)
  (let ((asm (cg-asm cg)))
    (define-label asm "_print_int")
    (emit-prologue asm)
    (emit-alloc-stack asm 32)

    (emit-mov-reg asm X9 X0)
    (emit-add-imm asm X10 SP 30)
    (emit-mov-imm asm X11 0)
    (emit-strb asm 11 10 0)
    (emit-sub-imm asm X10 X10 1)
    (emit-mov-imm asm X12 10)

    (emit-cmp-imm asm X9 0)
    (emit-bcond asm COND-GE "_print_int_positive")
    (emit-neg asm X9 X9)
    (emit-mov-imm asm X13 1)
    (emit-b asm "_print_int_loop")

    (define-label asm "_print_int_positive")
    (emit-mov-imm asm X13 0)
    (emit-cmp-imm asm X9 0)
    (emit-bcond asm COND-NE "_print_int_loop")

    (emit-mov-imm asm X11 48)
    (emit-strb asm 11 10 0)
    (emit-b asm "_print_int_final")

    (define-label asm "_print_int_loop")
    (emit-cmp-imm asm X9 0)
    (emit-bcond asm COND-EQ "_print_int_check_neg")

    (emit-udiv asm X14 X9 X12)
    (emit-msub asm X11 X14 X12 X9)
    (emit-add-imm asm X11 X11 48)
    (emit-strb asm 11 10 0)
    (emit-sub-imm asm X10 X10 1)
    (emit-mov-reg asm X9 X14)
    (emit-b asm "_print_int_loop")

    (define-label asm "_print_int_check_neg")
    (emit-cmp-imm asm X13 0)
    (emit-bcond asm COND-EQ "_print_int_done")
    (emit-mov-imm asm X11 45)
    (emit-strb asm 11 10 0)
    (emit-b asm "_print_int_final")

    (define-label asm "_print_int_done")
    (emit-add-imm asm X10 X10 1)

    (define-label asm "_print_int_final")
    ; Would call rt_print_str here
    (emit-mov-reg asm X0 X10)
    ; For now just return, would need runtime call

    (emit-free-stack asm 32)
    (emit-epilogue asm)))

; ===== Main Generation Entry Point =====

(define (generate-program cg ast)
  (let ((asm (cg-asm cg)))
    ; Jump to main
    (emit-b asm "_main")

    ; Generate print helper
    (generate-print-int-helper cg)

    ; Generate main
    (define-label asm "_main")
    (emit-prologue asm)

    ; Generate top-level expressions
    (generate-begin cg ast)

    ; Return 0
    (emit-mov-imm asm X0 0)
    (emit-epilogue asm)))

; ===== Test =====
; Generate code for a simple program

(define test-cg (make-codegen))
(define test-ast (list 42))  ; Just the number 42

(generate-program test-cg test-ast)
(print (asm-pos (cg-asm test-cg)))  ; Print total bytes generated
