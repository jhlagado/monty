printAnsiSeq:
    call printStr
    .cstr ESC,"["
    ret
    
printAnsiStr:
    call printAnsiSeq
    jp printStr

; hl = number to print in decimal
printAnsiNum:
    push hl                     ; save hl
    ld de,(vBufPtr)             ; de' = buffer* 
    exx                                                   
    pop hl                      ; restore hl
    call formatDec
    exx                         ; restore de = buffer*
    ld a,0                      ; append NUL to buffer
    ld (de),a
    inc de                      ; string*++, 
    ld (vBufPtr),de             ; update buffer* with buffer*'
    ld hl,BUFFER
    ld (vBufPtr),hl             ; reset vBufPtr to vHeapPtr
    jp putstr

; clears screen and sets cursor to 0,0
clearScreen: 
    call printAnsiStr
    .cstr "2J"                  ; falls through
; set cursor to 0, 0
home:
    call printAnsiStr
    .cstr "H"
    ret
    
; a = 0:toEnd 1:toStart 2:entireLine
clearLine: 
    add "0"                     ; ascii
    ex af,af'                   ; save a
    call printAnsiSeq
    ex af,af'                   ; restore
    call putChar
    ld a,'K'
    jp putChar

; h = column, l = row 
gotoXY: 
    exx                         ; save hl
    call printAnsiSeq
    exx                         ; restore hl
    ld a,l                      ; a = row
    exx                         ; save hl
    call printNum
    ld a,';'
    call putChar
    exx                         ; restore hl
    ld a,h                      ; a = col
    call printNum
    ld a,'H'
    jp putChar

; h = "A"|"B"|"C"|"D"   where A = Up, B = Down, C = Forward D = Back
; l = amount
move:
    exx                         ; save hl
    call printAnsiStr
    exx                         ; restore hl
    ld a,l
    exx                         ; save hl
    call printNum
    exx                         ; restore hl
    ld a,h
    jp putChar

; a = 0:normal 1:bold 2:low 4:underline 5:blink 7:reverse 
textStyle:
    add "0"                     ; ascii
    ex af,af'                   ; save a
    call printAnsiSeq
    ex af,af'                   ; restore
    call putChar
    ld a,'m'
    jp putChar

hideCursor: 
    call printAnsiStr
    .cstr "?25l"	
    ret

showCursor() 
    call printAnsiStr
    .cstr "?25h"	
    ret

