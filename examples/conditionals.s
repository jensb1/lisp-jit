.section __TEXT,__text
.global _main
.align 4

_main:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    adrp x0, str0@PAGE
    add x0, x0, str0@PAGEOFF
    bl _print_str
    mov x0, #10
    str x0, [sp, #-16]!
    mov x0, #5
    ldr x1, [sp], #16
    cmp x1, x0
    cset x0, gt
    cmp x0, #0
    b.eq else0
    mov x0, #1
    b endif1
else0:
    mov x0, #0
endif1:
    bl _print_int
    mov x0, #10
    str x0, [sp, #-16]!
    mov x0, #5
    ldr x1, [sp], #16
    cmp x1, x0
    cset x0, lt
    cmp x0, #0
    b.eq else2
    mov x0, #1
    b endif3
else2:
    mov x0, #0
endif3:
    bl _print_int
    mov x0, #42
    str x0, [sp, #-16]!
    mov x0, #42
    ldr x1, [sp], #16
    cmp x1, x0
    cset x0, eq
    cmp x0, #0
    b.eq else4
    mov x0, #100
    b endif5
else4:
    mov x0, #200
endif5:
    bl _print_int
    mov x0, #10
    str x0, [sp, #-16]!
    mov x0, #10
    ldr x1, [sp], #16
    cmp x1, x0
    cset x0, ge
    cmp x0, #0
    b.eq else6
    mov x0, #1
    b endif7
else6:
    mov x0, #0
endif7:
    bl _print_int
    mov x0, #5
    str x0, [sp, #-16]!
    mov x0, #10
    ldr x1, [sp], #16
    cmp x1, x0
    cset x0, le
    cmp x0, #0
    b.eq else8
    mov x0, #1
    b endif9
else8:
    mov x0, #0
endif9:
    bl _print_int
    mov x0, #15
    adrp x1, _global_x@PAGE
    add x1, x1, _global_x@PAGEOFF
    str x0, [x1]
    adrp x0, _global_x@PAGE
    add x0, x0, _global_x@PAGEOFF
    ldr x0, [x0]
    str x0, [sp, #-16]!
    mov x0, #20
    ldr x1, [sp], #16
    cmp x1, x0
    cset x0, gt
    cmp x0, #0
    b.eq else10
    mov x0, #1
    b endif11
else10:
    adrp x0, _global_x@PAGE
    add x0, x0, _global_x@PAGEOFF
    ldr x0, [x0]
    str x0, [sp, #-16]!
    mov x0, #10
    ldr x1, [sp], #16
    cmp x1, x0
    cset x0, gt
    cmp x0, #0
    b.eq else12
    mov x0, #2
    b endif13
else12:
    mov x0, #3
endif13:
endif11:
    bl _print_int
    mov x0, #0
    ldp x29, x30, [sp], #16
    ret

_print_int:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    sub sp, sp, #32
    mov x9, x0
    add x10, sp, #30
    mov x11, #0
    strb w11, [x10]
    sub x10, x10, #1
    mov x12, #10
    cmp x9, #0
    b.ge _print_int_positive
    neg x9, x9
    mov x13, #1
    b _print_int_loop
_print_int_positive:
    mov x13, #0
    cmp x9, #0
    b.ne _print_int_loop
    mov w11, #48
    strb w11, [x10]
    b _print_int_final
_print_int_loop:
    cmp x9, #0
    b.eq _print_int_check_neg
    udiv x14, x9, x12
    msub x11, x14, x12, x9
    add x11, x11, #48
    strb w11, [x10]
    sub x10, x10, #1
    mov x9, x14
    b _print_int_loop
_print_int_check_neg:
    cmp x13, #0
    b.eq _print_int_done
    mov w11, #45
    strb w11, [x10]
    b _print_int_final
_print_int_done:
    add x10, x10, #1
_print_int_final:
    mov x0, x10
    bl _puts
    add sp, sp, #32
    ldp x29, x30, [sp], #16
    ret

_print_str:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    bl _puts
    ldp x29, x30, [sp], #16
    ret

.section __DATA,__data
fmt_int: .asciz "%lld\n"
fmt_str: .asciz "%s\n"
str0: .asciz "Conditionals:"
_global_x: .quad 0