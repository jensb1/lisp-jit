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
    mov x0, #20
    ldr x1, [sp], #16
    add x0, x1, x0
    bl _print_int
    mov x0, #100
    str x0, [sp, #-16]!
    mov x0, #42
    ldr x1, [sp], #16
    sub x0, x1, x0
    bl _print_int
    mov x0, #6
    str x0, [sp, #-16]!
    mov x0, #7
    ldr x1, [sp], #16
    mul x0, x1, x0
    bl _print_int
    mov x0, #100
    str x0, [sp, #-16]!
    mov x0, #4
    ldr x1, [sp], #16
    sdiv x0, x1, x0
    bl _print_int
    adrp x0, str1@PAGE
    add x0, x0, str1@PAGEOFF
    bl _print_str
    mov x0, #1
    str x0, [sp, #-16]!
    mov x0, #2
    ldr x1, [sp], #16
    add x0, x1, x0
    str x0, [sp, #-16]!
    mov x0, #3
    ldr x1, [sp], #16
    add x0, x1, x0
    str x0, [sp, #-16]!
    mov x0, #4
    ldr x1, [sp], #16
    add x0, x1, x0
    str x0, [sp, #-16]!
    mov x0, #5
    ldr x1, [sp], #16
    add x0, x1, x0
    bl _print_int
    mov x0, #2
    str x0, [sp, #-16]!
    mov x0, #3
    ldr x1, [sp], #16
    mul x0, x1, x0
    str x0, [sp, #-16]!
    mov x0, #4
    ldr x1, [sp], #16
    mul x0, x1, x0
    bl _print_int
    adrp x0, str2@PAGE
    add x0, x0, str2@PAGEOFF
    bl _print_str
    mov x0, #3
    str x0, [sp, #-16]!
    mov x0, #4
    ldr x1, [sp], #16
    mul x0, x1, x0
    str x0, [sp, #-16]!
    mov x0, #5
    str x0, [sp, #-16]!
    mov x0, #6
    ldr x1, [sp], #16
    mul x0, x1, x0
    ldr x1, [sp], #16
    add x0, x1, x0
    bl _print_int
    mov x0, #10
    str x0, [sp, #-16]!
    mov x0, #20
    ldr x1, [sp], #16
    add x0, x1, x0
    str x0, [sp, #-16]!
    mov x0, #3
    ldr x1, [sp], #16
    sdiv x0, x1, x0
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
    b _print_int_done
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
str0: .asciz "Basic arithmetic:"
str1: .asciz "Chained operations:"
str2: .asciz "Nested expressions:"