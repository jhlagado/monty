.macro expect,msg1,val1

    pop HL
    push HL
    ld DE,val1
    or A
    sbc HL,DE
    ld A,L
    or H
    jp Z,expect%%M

    call printStr
    .cstr "Code: ",msg1

    call printStr
    .cstr "\r\n\r\nExpected: "
    ld hl,val1
    ; call prtdec
    push hl
    call run
    .cstr "."

    call printStr
    .cstr "\r\n\r\nActual: "
    pop hl
    ; push hl
    ; call prtdec
    push hl
    push hl
    call run
    .cstr ". `(#`.s .h `)\r\n`.s"
    ; pop hl
    ; call printStr
    ; .cstr " (#"
    ; call prthex
    ; call printStr
    ; .cstr ")\r\n"

    call flushBuffer
    halt
    .cstr
expect%%M:
    pop HL
.endm

.macro test,code1,val1
    ld SP,STACK
    call coldInit
    call execStr
    .cstr code1
    call flushBuffer
    expect code1,val1
.endm

.macro kall,label1
    db msb(label1 / 2) + $80
    db lsb(label1 / 2)
.endm
