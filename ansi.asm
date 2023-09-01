ansiPrintSeq:
    call printStr
    .db ESC,"[",0
    ret
    
; clears screen and sets cursor to 0,0
ansiClearScreen: 
    call printStr
    .db ESC,"["
    .db "2J",0                  ; falls through
    ; call ansiPrintStr
    ; .cstr "H"
    ret
    
; a = 0:toEnd 1:toStart 2:entireLine
ansiClearLine: 
    add a,"0"                     ; ascii
    ex af,af'                   ; save a
    call ansiPrintSeq
    ex af,af'                   ; restore
    call putChar
    ld a,'K'
    jp putChar

; h = column, l = row 
ansiGoto: 
    exx                         ; save hl
    call ansiPrintSeq
    exx                         ; restore hl
    ld a,h                      ; save column
    ex af,af'                   
    ld h,0                      ; print row
    call printNum
    ld a,';'
    call putChar
    ex af,af'                   ; restore column
    ld l,h                      ; print column
    ld h,0                      
    call printNum
    ld a,"H"
    jp putChar

; h = "A":Up "B":Down "C":Forward "D":Back
; l = amount
ansiMove:
    exx                         ; save hl
    call ansiPrintSeq
    exx                         ; restore hl
    ld a,h
    ex af,af'                   ; save a
    ld h,0                      ; print amount
    call printNum
    ex af,af'                   ; restore a
    jp putChar                  ; print direction code

; a = 0:normal 1:bold 2:low 4:underline 5:blink 7:reverse 
ansiTextStyle:
    add a,"0"                   ; ascii
    ex af,af'                   ; save a
    call ansiPrintSeq
    ex af,af'                   ; restore
    call putChar
    ld a,"m"
    jp putChar

; a = "h":show "l":hide
ansiCursorShow:
    ex af,af'                   ; save a
    call printStr
    .db ESC,"["
    .db "?25l",0	
    ex af,af'                   ; restore
    jp putChar
