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
    push hl
    push hl
    call run
    .cstr ". /h./d"
    ; .cstr ". `(`.s /h./d `)\r\n`.s"
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
