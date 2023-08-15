
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


================

Here's an "if" condition

```
3 2 > { `hello` } ?
```

If 3 is greater than 2 then print hello

- putting text between \` and \` means print this text
- `?` means: if the condition is true then execute the block.
- if..else is done using the `?` operator

```
3 2 > { `greater` } { `less than` } ?
```

Loops are infinite and are represented with ( )
You run them with ^
You can terminate them with /br which will break the loop if a condition is false.
Counting to 10

```
1 i = ( i . i 10 <= /br )^
> 1 2 3 4 5 6 7 8 9 10
```

There are other commands in Monty which do not use symbols. These use a / followed
by one or two letters. Example, to `xor` two values:

```
$55 $FF /x .
> 255
```

To show as hex use /hx and /dx for decimal

```
255 /hx .
> $FF
```

Arrays are simple and can be byte or word sized.
Create an array and store in A

```
[ 10 20 30 ] A=
```

To access the second element (index 1) of this array

```
A 1# .
>20
```

================

