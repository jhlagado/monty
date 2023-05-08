.macro expect,msg1,val1

    pop HL
    push HL
    ld DE,val1
    or A
    sbc HL,DE
    ld A,L
    or H
    jr Z,expect%%M

    call printStr
    .cstr "Code: ",msg1

    call printStr
    .cstr "\r\n\r\nExpected: "
    ld HL,val1
    call prtdec

    call printStr
    .cstr "\r\n\r\nActual: "
    pop hl
    push hl
    call prtdec
    pop hl
    call printStr
    .cstr " (#"
    call prthex
    call printStr
    .cstr ")\r\n"

    halt
    .cstr
expect%%M:
    pop HL
.endm

.macro test,code1,val1
    ld SP,STACK
    call init
    call execStr
    .cstr code1
    expect code1,val1
.endm

.macro kall,label1
    db msb(label1 / 2) + $80
    db lsb(label1 / 2)
.endm


