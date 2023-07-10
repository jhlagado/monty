.macro expect
.endm

.macro test
    ld SP,STACK
    call coldInit
    call execStr
    db %%1,0
    call flushBuffer
    pop HL
    push HL
    ld DE,%%2
    or A
    sbc HL,DE
    ld A,L
    or H
    jp Z,expect%%M

    call printStr
    .cstr "Code: ",%%1

    call printStr
    .cstr "\r\n\r\nExpected: "
    ld hl,%%2
    push hl
    push hl
    call run
    .cstr ". /h./d"
    call flushBuffer

    call printStr
    .cstr "\r\n\r\nActual: "
    pop hl
    push hl
    push hl
    call run
    .cstr ". /h./d `\r\n`.s"
    call flushBuffer
    halt
expect%%M:
    pop HL
.endm
