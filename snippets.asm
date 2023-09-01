### Stack frame

```
    arg0                              -- 0th arg
    arg1
     :
    argn                              -- nth arg
    loc0                              -- 0th local
    loc1
     :
    locn                              -- last local
    IP                                -- IP (saved interpreter ptr, return)
    arg_list*                         -- arg_list*
    first_arg*                        -- first_arg*
    BP                                -- BP (saved base ptr)           <-- iy
    res0                              -- 0th result
    res1
     :
    resn                              -- last result.             <-- sp
```

### Control codes

```
    ^@  0 NUL
    ^A  1 SOH
    ^B  2 STX
    ^C  3 ETX
    ^D  4 EOT
    ^E  5 ENQ
    ^F  6 ACK
    ^G  7 BEL
    ^H  8 BS
    ^I  9 TAB
    ^J 10 LF
    ^K 11 VT
    ^L 12 FF
    ^M 13 CR
    ^N 14 SO
    ^O 15 SI
    ^P 16 DLE
    ^Q 17 DC1
    ^R 18 DC2
    ^S 19 DC3
    ^T 20 DC4
    ^U 21 NAK
    ^V 22 SYN
    ^W 23 ETB
    ^X 24 CAN
    ^Y 25 EM
    ^Z 26 SUB
    ^[ 27 ESC
    ^\ 28 FS
    ^] 29 GS
    ^^ 30 RS
    ^_ 31 US
```

; str -- num
hash:
    pop hl
    push bc
    ld bc,hl
    call hashStr
    pop bc
    push hl
    jp (ix)

sqrt1:
    pop hl
    push bc
    call squareRoot
    ld (vRemain),bc
    pop bc
    push de
    jp (ix)

; hash C-string 
; BC = str
; HL = hash
hashStr:
    ld (vHashStr),bc                    ; store source string
    ld hl,0                             
hashStr1:    
    ld a,(bc)                           ; load next char
    inc bc
    cp 0                                ; NUL?
    ret z                     
hashStr2:
    ld d,0
    ld e,a 
    add hl,de
    ld de,hl                            ; hl *= 193 (11000001)
    add hl,hl                           ; shift left
    add hl,de                           ; add
    add hl,hl                           ; shift left
    add hl,hl                           ; shift left
    add hl,hl                           ; shift left
    add hl,hl                           ; shift left
    add hl,hl                           ; shift left
    add hl,hl                           ; shift left
    add hl,de                           ; add
    jr hashStr1

; squareroot
; Input: HL = value
; Result: DE = square root BC = remainder

squareRoot:
    ld bc,0800h   
    ld e,c        
    xor a         
squareRoot1:        
    add hl,hl     
    rl c          
    adc hl,hl     
    rl c          
    jr nc,$+4     
    set 0,l       
    ld a,e        
    add a,a       
    ld e,a        
    add a,a       
    bit 0,l       
    jr nz,$+5     
    sub c         
    jr nc,squareRoot4     
    ld a,c         
    sub e              
    inc e          
    sub e           
    ld c,a         
squareRoot4:
    djnz squareRoot1
    bit 0,l       
    jr z,squareRoot5         
    inc b         
squareRoot5:
    ld d,0
    ret          

; /pk print stack
; -- 
printStack:
    ld (vTemp1),bc
    call printStr
    .cstr "=> "
    ld hl,STACK
    sbc hl,sp
    srl h
    rr l
    ld bc,hl
    ld hl,STACK
    jr printStack2
printStack1:
    dec bc
    dec hl
    ld d,(hl)
    dec hl
    ld e,(hl)
    ex de,hl    
    call prthex
    ex de,hl
    ld a," "
    call putchar
printStack2:
    ld a,c
    or b
    jr nz,printStack1
    call prompt
    ld bc,(vTemp1)
    jp (ix)

; /pb printBuffer
; --
; prints chars in buffer from /vB to /vb. Resets /vb to /vB

FUNC printBuffer, 0, "a"
.cstr "{/vB /vb/vB- /pc /vB/vb=}"   ; block

FUNC f3, 0, ""                                    
db "{"
db    ":kt{"                            
db      ":dt{"                          ; return talkback to receive data
db        "0 2 %k^"                         
db      "}; 0 %k^"                      ; init sink
db    "};" 
db "}" 
db 0

FUNC f4, 1, "spT"                                     
db "{"
db    ":dt{"                        ; *** return talkback to receive data ; $56AA
db      "0%t==/br"                  ; break if type = 2
db      "0 0 %d^"                 ; 0 or 1: get next src data item
db    "}; 0 %s^" 
db "}" 
db 0


; _ func
; -- func*
underscore:
colon:
lambda:
    push ix
    ld ix,lambda1
    jp arglist
lambda1:
    inc bc
    ld ix,lambda2
    jp blockStart
lambda2:    
    ld ix,lambda3
    jp createFunc
lambda3:
    pop hl
    pop ix
    push hl
    jp (ix)

; breakout the machine code from inside lambda
; accesses first arg

f1:
    call go
    dw NUL                      
    dw f1x                      
    dw $+2
    db 0                
    .pstr "a"
f1x:
    db "{`h`"
    db 0
    
    ld e,(iy+2)                 ; de = first_arg*   
    ld d,(iy+3)
    ex de,hl
    dec hl
    ld d,(hl)
    dec hl
    ld e,(hl)
    ld a,e
    call putchar

    ld bc,f1y-1
    jp (ix)
f1y:
    db "`y`}"
    db 0
