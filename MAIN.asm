; *************************************************************************
;
;  Monty programming language for the Z80 
;
;  by John Hardy 2023
;
;  GNU GENERAL PUBLIC LICENSE    Version 3, 29 June 2007
;
;  see the LICENSE file in this repo for more information 
;
;  Incorporating code from the MINT project by Ken Boak and Craig Jones.
;  Inspiration from Charles H. Moore, Peter Jakacki and André Staltz
;
; *****************************************************************************

TRUE        equ     -1		    ; C-style true
FALSE       equ     0
NUL         equ     0           ; exit code
DQ          equ     $22         ; " double quote char
CTRL_C      equ     3
CTRL_E      equ     5
CTRL_H      equ     8
CTRL_J      equ     10
CTRL_L      equ     12
CTRL_P      equ     16
CTRL_S      equ     19
ESC         equ     27          

; macros for inlining a onty function in assembly
; follow immediately with a null terminated block of Monty code
.macro FUNC,name,numLocals,argsStr
name:
    call go
    dw NUL                      ; NUL closure
    dw name%%M                      
    dw $+2
    db numLocals                ; num_locals
    .pstr argsStr
name%%M:
.endm

.macro PERFORM,name
    ld ix,perform%%M
    jp name
perform%%M:
.endm

.org ROMSTART + $180		    ; 0+180 put monty code from here	

;********************** PAGE 1 BEGIN *********************************************

opcodes:                         
    DB lsb(bang_)               ; !     1
    DB lsb(dquote_)             ; "     2
    DB lsb(hash_)               ; #     1
    DB lsb(dollar_)             ; $     1
    DB lsb(percent_)            ; %     1
    DB lsb(amper_)              ; &     1
    DB lsb(quote_)              ; '     2
    DB lsb(lparen_)             ; (     1
    DB lsb(rparen_)             ; )     1
    DB lsb(star_)               ; *     1
    DB lsb(plus_)               ; +     1
    DB lsb(comma_)              ; ,     2
    DB lsb(minus_)              ; -     1
    DB lsb(dot_)                ; .     2
    DB lsb(slash_)              ; /	    1
    DB lsb(num_)                ; 0     1
    DB lsb(num_)                ; 1     
    DB lsb(num_)                ; 2     
    DB lsb(num_)                ; 3     
    DB lsb(num_)                ; 4     
    DB lsb(num_)                ; 5     
    DB lsb(num_)                ; 6     
    DB lsb(num_)                ; 7     
    DB lsb(num_)                ; 8     
    DB lsb(num_)                ; 9     
    DB lsb(colon_)              ; :     2
    DB lsb(semicolon_)          ; ;     2
    DB lsb(lt_)                 ; <     1
    DB lsb(eq_)                 ; =     1
    DB lsb(gt_)                 ; >     1 
    DB lsb(question_)           ; ?     1  
    DB lsb(at_)                 ; @     2
    DB lsb(upcase_)             ; A     1
    DB lsb(upcase_)             ; B     
    DB lsb(upcase_)             ; C     
    DB lsb(upcase_)             ; D     
    DB lsb(upcase_)             ; E     
    DB lsb(upcase_)             ; F     
    DB lsb(upcase_)             ; G     
    DB lsb(upcase_)             ; h     
    DB lsb(upcase_)             ; I     
    DB lsb(upcase_)             ; J     
    DB lsb(upcase_)             ; K     
    DB lsb(upcase_)             ; L     
    DB lsb(upcase_)             ; M     
    DB lsb(upcase_)             ; N     
    DB lsb(upcase_)             ; O     
    DB lsb(upcase_)             ; p     
    DB lsb(upcase_)             ; Q     
    DB lsb(upcase_)             ; R     
    DB lsb(upcase_)             ; S     
    DB lsb(upcase_)             ; T     
    DB lsb(upcase_)             ; U     
    DB lsb(upcase_)             ; V     
    DB lsb(upcase_)             ; W     
    DB lsb(upcase_)             ; X     
    DB lsb(upcase_)             ; Y     
    DB lsb(upcase_)             ; Z     
    DB lsb(lbrack_)             ; [     2
    DB lsb(backslash_)          ; \     2
    DB lsb(rbrack_)             ; ]     2
    DB lsb(caret_)              ; ^     1
    DB lsb(underscore_)         ; _     2
    DB lsb(grave_)              ; `     2   	    
    DB lsb(lowcase_)            ; a     1
    DB lsb(lowcase_)            ; b  
    DB lsb(lowcase_)            ; c  
    DB lsb(lowcase_)            ; d  
    DB lsb(lowcase_)            ; e  
    DB lsb(lowcase_)            ; f  
    DB lsb(lowcase_)            ; g  
    DB lsb(lowcase_)            ; h  
    DB lsb(lowcase_)            ; i  
    DB lsb(lowcase_)            ; j  
    DB lsb(lowcase_)            ; k  
    DB lsb(lowcase_)            ; l  
    DB lsb(lowcase_)            ; m  
    DB lsb(lowcase_)            ; n  
    DB lsb(lowcase_)            ; o  
    DB lsb(lowcase_)            ; p  
    DB lsb(lowcase_)            ; q  
    DB lsb(lowcase_)            ; r  
    DB lsb(lowcase_)            ; s  
    DB lsb(lowcase_)            ; t  
    DB lsb(lowcase_)            ; u  
    DB lsb(lowcase_)            ; v  
    DB lsb(lowcase_)            ; w  
    DB lsb(lowcase_)            ; x  
    DB lsb(lowcase_)            ; y  
    DB lsb(lowcase_)            ; z     
    DB lsb(lbrace_)             ; {     1
    DB lsb(pipe_)               ; |     1
    DB lsb(rbrace_)             ; }     1
    DB lsb(tilde_)              ; ~     1   

;********************** PAGE 1 END *********************************************

; ***********************************************************************
; Initial values for system vars		
; ***********************************************************************		
isysVars:			            
    dw TIB                      ; vTIBPtr pointer into TIB
    dw BUFFER                   ; vBufPtr pointer into BUF
    dw HEAP                     ; vHeapPtr \h start of the free mem
    dw NUL                      ; vRecurPtr
    db 2                        ; vDataWidth in bytes of array operations (default 1 byte) 
    db 10                       ; vNumBase = 10
    db "$"                      ; vHexPrefix
    db TRUE                     ; vEcho
    db FALSE                    ; vStrMode
    db 0
    db 0                         
    db 0                         
    db 0                         
    db 0                         

; **********************************************************************			 
; title string (also used by warm boot) 
; **********************************************************************

titleStr:
    .cstr ESC,"[2JMonty V0.1\r\n",0,0,0

;********************** PAGE 2 BEGIN ***********************************

colon_:
    jp (ix)

comma_:
    jp comma
    
dollar_:
    jp dollar

dquote_:
quote_:
    jp quote

dot_:  
    jp dot

percent_:        
    jp percent 

question_:
    jp question

lparen_:
    jp lbrace

slash_:
    jp slash

num_:    
    jp  num

bang_:				             
bang:				            ; logical invert, any non zero value 
    inc bc
    ld a,(bc)
    cp "="
    jr nz,not
    pop hl
    pop de
    jp notequals
not:
    dec bc
    ld hl,0                     ; is considered true
    jr eq1    
eq_:    
eq:
    inc bc
    ld a,(bc)
    cp "="
    jr z,eq0
    dec bc
    jp assign
eq0:
    pop hl
eq1:
    pop de
    jp equals

gt_:
gt:
    inc bc
    ld a,(bc)
    cp ">"
    jp z,shiftRight
    pop de
    pop hl
    jr lt1
lt_:
lt:
    inc bc
    ld a,(bc)
    cp "<"
    jp z,shiftLeft
    pop hl
    pop de
lt1:
    cp "="
    jp z,lessthaneq
    dec bc
    jp lessthan

; index of an array, based on vDataWidth 22
; array* num -- value    ; also sets vPointer to address 
hash_:    
semicolon_:
semicolon:
arrayIndex:
    pop hl                              ; hl = index  
    pop de                              ; de = array
    ld a,(vDataWidth)                   ; a = data width
    dec a
    jr z,arrayIndex1
arrayIndex0:
    add hl,hl                           ; if data width = 2 then double 
arrayIndex1:
    add hl,de                           ; add addr
    jp variable

;                               4
rparen_:
    ; jp rparen
rparen:
    ld c,(iy+8)                 ; IP = block* just under stack frame
    ld b,(iy+9)
    jp (ix)


; & and                          14
; a b -- c
amper_:
amper:                          
and:
    pop de                      ; Bitwise and the top 2 elements of the stack
    pop hl     
    ld a,e        
    and l           
    ld l,a        
    ld a,d        
    and h           
    ld h,a        
    jr add3        

; - sub                          
; a b -- c
; -- sub                          
; b1 -- b2
; -= sub                          
; a b1 -- b2

minus_:
minus:
    inc bc                      ; check if sign of a number
    ld a,(bc)
    dec bc
    cp "0"
    jr c,sub
    cp "9"+1
    jp c,num    
sub:                            ; Subtract the value 2nd on stack from top of stack 
    pop hl                      ; hl = arg_b
    inc bc
    cp "-"
    jr nz,sub1
    dec hl                      ; --
    jp assign0
sub1:
    pop de                      ; de = arg_a
    cp "="
    jr z,sub2
    dec bc                      ; -                    
    ex de,hl                     
sub2:
    or a                        ; -=
    sbc hl,de    
sub3:
    cp "="
    jp z,assign0
    push hl        
    jp (ix)    

star_:                          ; 21    
star:
    inc bc                      ; check for ** spread
    ld a,(bc)
    cp "*"
    jp z,spread
    dec bc
mul:        
    pop  de                     ; get first value
    pop  hl
mul2:
    push bc                     ; Preserve the IP
    ld bc,hl                    ; bc = 2nd value
    ld hl,0
    ld a,16
mul3:
    add hl,hl
    rl e
    rl d
    jr nc,$+6
    add hl,bc
    jr nc,$+3
    inc de
    dec a
    jr nz,mul3
	pop bc			            ; Restore the IP
    jp add3

; + add                         25
; a b -- c
plus_:                           
plus:
add:
    inc bc
    ld a,(bc)
    cp "+"                      ; ++ increment variable
    jr nz,add1
    pop hl
    inc hl
    jp assign0
add1:
    dec bc
    pop de                      ; second term
    pop hl                      ; first term
    add hl,de    
add3:
    inc bc
    ld a,(bc)
    cp "="
    jr z,add4
    dec bc
    push hl        
    jp (ix)    
add4:
    jp assign0

;                               18
upcase_:
upcase:
    ld a,(bc)                   ; a = identifier char
    sub 'A'                     ; 'A' = 0
    jr ident1

;********************** PAGE 2 END *********************************************
.align $100
;********************** PAGE 3 BEGIN *********************************************

lowcase_:
lowcase:
    ld a,(bc)
    sub 'a' 
    add a,26
ident1:
    add a,a                     ; l = a * 2                             
    ld l,a
    ld h,msb(VARS)     
    ld (vPointer),hl            ; store address in setter    
    ld e,(hl)
    inc hl
    ld d,(hl)
    push de
    jp (ix)

; [                             14
lbrack_:
lbrack:
arrayStart:
    ld de,0                     ; create stack frame
    push de                     ; push null for IP
    ld e,(iy+4)                 ; push arg_list* from parent stack frame
    ld d,(iy+5)                 ; 
    push de                     ; 
    ld e,(iy+2)                 ; push first_arg* from parent stack frame
    ld d,(iy+3)                 ; 
    push de                     ; 
    push iy                     ; push BP  
    ld iy,0                     ; BP = SP
    add iy,sp
    jp (ix)

; \                             19
backslash_:
backslash:
lambda:
    push ix
    call parseArgs
lambda1:
    inc bc                      ; arg_list must ve immediately followed by {
    ld a,(bc)
    cp " "+1                    ; skip white space
    jr c,lambda1
    cp "{"
    jr z,lambda2
    ld hl,2                     ; error 2: parse error
    jp error
lambda2:
    call parseBlock
    call createFunc
    pop hl
    pop ix
    push hl
    jp (ix)

rbrack_:
rbrack:
arrayEnd:                       ; 53
    ld d,iyh                    ; de = BP
    ld e,iyl
    ld (vTemp1),bc              ; save IP
    ld hl,de                    ; hl = de = BP
    or a 
    sbc hl,sp                   ; hl = array count (items on stack)
    srl h                       ; 
    rr l                        
    ld bc,hl                    ; bc = count
    ld hl,(vHeapPtr)            ; hl = array[-4]
    ld (hl),c                   ; write num items in length word
    inc hl
    ld (hl),b
    inc hl                      ; hl = array[0], bc = count
                                ; de = BP, hl = array[0], bc = count
    jr arrayEnd3
arrayEnd1:                        
    ld a,(iy-2)                 ; a = lsb of stack item
    ld (hl),a                   ; write lsb of array item
    inc hl                      ; move to msb of array item
    ld a,(vDataWidth)           ; vDataWidth=1? 
    dec a
    jr z,arrayEnd2
    ld a,(iy-1)                 ; a = msb of stack item
    ld (hl),a                   ; write msb of array item
    inc hl                      ; move to next word in array
arrayEnd2:
    dec iy                      ; move to next word on stack
    dec iy
    dec bc                      ; dec items count
arrayEnd3:
    ld a,c                      ; if not zero loop
    or b
    jr nz,arrayEnd1
    ex de,hl                    ; de = end of array, hl = BP 
    ld sp,hl                    ; sp = BP
    pop hl                      ; de = end of array, hl = old BP
    ex de,hl                    ; iy = de = old BP, hl = end of array
    ld iyh,d
    ld iyl,e
    pop de                      ; pop arg_list (discard)
    pop de                      ; pop first_arg* (discard)
    pop de                      ; pop IP (discard)
    ld de,(vHeapPtr)            ; de = array[-2]
    inc de
    inc de
    push de                     ; return array[0]
    ld (vHeapPtr),hl            ; move heap* to end of array
    ld bc,(vTemp1)              ; restore IP
    jp (ix)

caret_: 		 
; execute a block of code which ends with } 116
; creates a root scope if BP == stack
; else uses outer scope 
caret:
go:				       
    pop de                      ; de = block*
go1:
    ld a,e                      ; if block* == null, exit
    or d
    jr nz,go2
    jp (ix)
go2:
    ld a,(de)
    cp "{"
    jp z,goBlock
    cp "("
    jp nz,goFunc
    push de                     ; push de just before stack frame
    jp z,goBlock

grave_:
grave:
printLiteral:
    inc bc                      ; move to first char
    ld de,(vBufPtr)             ; de = buffer*
    jr printLiteral1
printLiteral0:
    ld (de),a                   ; a -> buffer*
    inc de                      ; string*++, 
    inc bc
printLiteral1:
    ld a,(bc)                   ; a <- string*
    cp "`"                      ; if ` exit loop
    jr nz,printLiteral0
    ld (vBufPtr),de             ; save buffer*' in pointer
    jp dotNext

lbrace_:
lbrace:
    call parseBlock
    jp (ix)

; | or                          11
; a b -- c
pipe_: 		 
pipe:
or:
    pop de                      ; Bitwise or the top 2 elements of the stack
    pop hl
    ld a,e
    or l
    ld l,a
    ld a,d
    or h
    ld h,a        
    jp add3        

rbrace_:
    jp rbrace

; ~ char                        8
tilde_:
    jp tilde

underscore_:
char:
    inc bc                      ; point to next char
    ld a,(bc)
    ld l,a
    ld h,0
    push hl
    jp (ix)  

at_:
at:
addr:
    ld de,(vPointer)
    ld hl,vPointer
    jp variable


;********************** PAGE 3 END *********************************************
.align $100
;********************** PAGE 4 BEGIN *********************************************

;                               67
dot:
print:
    call cmdTable
    db "a",0                    ; .a print array
    dw printArray
    db "c",0                    ; .c print char
    dw printChar
    db "s",0                    ; .s print string
    dw printString
    dw 0                        ; .  print number, fall through
    dw printNumber

; .c print char             
; char -- 
printChar:
    pop hl                      ; a = char
    ld a,l
    ld de,(vBufPtr)             ; de = buffer*
    ld (de),a
    inc de
    ld (vBufPtr),de             ; save buffer*'
    jp dotNext

; .s print string             
; string* --
printString:
    pop hl                      ; hl = string*
    ld de,(vBufPtr)             ; de = buffer*
    jr printString1
printString0:
    ld (de),a                   ; a -> buffer*
    inc de                      ; string*++, 
    inc hl
printString1:
    ld a,(hl)                   ; a <- string*
    or a                        ; if NUL exit loop
    jr nz,printString0
    ld (vBufPtr),de             ; save buffer*' in pointer
    jp dotNext

; . print decimal
; value --                      
printNumber:        
    ld a,(vNumBase)
    cp 16
    jp z,printHex              ; else falls through
    jp printDec

; print decimal                 ; 70
; value --                      
printDec:        
    ld de,(vBufPtr)             ; de'= buffer* bc' = IP
    exx                          
    pop hl                      ; hl = value
    ld a,(vDataWidth)
    dec a
    jr nz,printDec1
    ld h,0
printDec1:    
    call printDec2
    exx                         ; de = buffer*' bc = IP
    ld a," "                    ; append space to buffer
    ld (de),a
    inc de                      ; string*++, 
    ld (vBufPtr),de             ; update buffer* with buffer*'
    jp dotNext

; hl = value
; de' = buffer*
; a, bc, de, hl destroyed
printDec2:    
    bit 7,h
    jr z,printDec3
    exx
    ld a,'-'
    ld (de),a
    inc de
    exx
    xor a  
    sub l  
    ld l,a
    sbc a,a  
    sub h  
    ld h,a
printDec3:        
    ld c,0                      ; leading zeros flag = false
    ld de,-10000
    call printDec4
    ld de,-1000
    call printDec4
    ld de,-100
    call printDec4
    ld e,-10
    call printDec4
    inc c                       ; flag = true for at least digit
    ld e,-1
    call printDec4
    ret
printDec4:	     
    ld b,'0'-1
printDec5:	    
    inc b
    add hl,de
    jr c,printDec5
    sbc hl,de
    ld a,'0'
    cp b
    jr nz,printDec6
    xor a
    or c
    ret z
    jr printDec7
printDec6:	    
    inc c
printDec7:	    
    ld a,b
    exx
    ld (de),a
    inc de
    exx
    ret

; buffer hex                    37
; value --                      

printHex:                      
    ld de,(vBufPtr)
    ld a,(vHexPrefix)           ; "$"
    or a                        ; skip if null
    jr z,printHex1
    ld (de),a
    inc de                      ; string*++, 
printHex1:
    pop hl                      ; hl = value
    ld a,(vDataWidth)
    dec a
    jr z,printHex2
    ld a,h
    call printHex3
printHex2:
    ld a,l
    call printHex3
    ld a," "                    ; append space to buffer
    ld (de),a
    inc de                      ; string*++, 
    ld (vBufPtr),de
    jp dotNext

printHex3:		     
    push af
	rra 
	rra 
	rra 
	rra 
    call printHex4
    pop af
printHex4:		
    and	0x0F
	add	a,0x90
	daa
	adc	a,0x40
	daa
	ld (de),a
    inc de                      ; string*++, 
	ret

;********************** PAGE 4 END *********************************************

.align $100
;********************** PAGE 5 BEGIN *********************************************

slash:
command:
    inc bc
    ld a,(bc)
    cp "/"                      ; // comment
    jp z,comment
    dec bc
    call charTable
    db lsb(command_a_)
    db lsb(command_b_)
    db 0
    db lsb(command_d_)
    db 0
    db lsb(command_f_)
    db 0
    db lsb(command_h_)
    db lsb(command_i_)
    db 0
    db 0
    db 0
    db lsb(command_m_)
    db 0
    db lsb(comand_o_)
    db lsb(command_p_)
    db lsb(command_q_)
    db lsb(command_r_)
    db lsb(command_s_)
    db lsb(comand_t_)
    db 0
    db lsb(command_v_)
    db lsb(command_w_)
    db lsb(command_x_)
    db 0
    db 0
    db lsb(command_default_)

; 12
command_a_:
    call cmdTable
    db "bs"                     ; /abs absolute
    dw absolute
    db "dr"                     ; /adr address of
    dw addrOf
    db "i",0                    ; /ai array iterator
    dw arrayIter
    db "ln"                     ; /al array length
    dw arrayLength
    db "s",0                    ; /as array size
    dw arraySize
    dw 0
    dw error1

command_b_:
    call cmdTable
    db "b",0                      ; /bb bye bye cold boot
    dw coldStart
    db "yt"                       ; /byt byte mode
    dw byteMode
    dw 0
    dw error1

command_d_:
    call cmdTable
    db "c",0                      ; /dc decimal
    dw decBase
    dw 0
    dw error1

command_f_:
    call cmdTable
    db "al"                       ; /fal false 
    dw false1
    db "d",0                      ; /fd fold
    dw fold
    db "or"                       ; /for forEach
    dw forEach
    db "s",0                      ; /fs funcSrc
    dw funcSrc
    db "t",0                      ; /ft filter
    dw filter
    db "1",0                      
    dw f1
    db "2",0                      
    dw f2
    db "3",0                      
    dw f3
    db "4",0                      
    dw f4
    dw 0
    dw error1                   

command_h_:
    call cmdTable
    db "x",0                      ; /hx hex
    dw hexBase
    dw 0
    dw error1                   

; 6
command_i_:
    call cmdTable
    db "n",0                      ; /in input
    dw input
    dw 0
    dw error1

command_m_:
    call cmdTable
    db "p",0                      ; /mp map
    dw map
    dw 0
    dw error1

comand_o_:
    call cmdTable
    db "ut",0                      ; /out out
    dw output
    dw 0
    dw error1
; 4
command_p_:
    call cmdTable
    dw 0
    dw error1

; 6
command_q_:
    call cmdTable
    db "it"                      ; /qit quit
    dw quit
    dw 0
    dw error1

command_r_:
    call cmdTable
    db "c",0                      ; /rc tail call optimisation
    dw recur
    db "em"                       ; /rem remainder
    dw remain
    db "ng"                       ; /rng range src
    dw rangeSrc
    dw 0
    dw error1

command_s_:
    jr command_s

comand_t_:
    jr command_t

command_v_:
    jr command_v

command_w_:
    jr command_w

command_x_:
    jr command_x
; 3
command_default_:
    jr command_default


;********************** PAGE 5 END *********************************************

command_s:    
    call cmdTable
    db "b",0
    dw stringBegin
    db "c",0
    dw stringCompare
    db "el"
    dw select
    db "e",0
    dw stringEnd
    db "i",0
    dw stringIter
    db "l",0
    dw stringLength
    db "s",0
    dw stringSize
    dw 0
    dw error1

command_t:
    call cmdTable
    db "ru"
    dw true1
    dw 0
    dw error1

command_v:
    call cmdTable
    db "b",0
    dw varBufPtr
    db "e",0
    dw varEcho
    db "h",0
    dw varHeapPtr
    db "t",0
    dw varTIBPtr
    db "x",0
    dw varHexPrefix
    db "B",0
    dw constBufStart
    db "H",0
    dw constHeapStart
    db "T",0
    dw constTIBStart
    dw 0
    dw error1

command_w:
    call cmdTable
    db "hi"                       ; /whi while true else break from loop
    dw while
    db "m",0                      ; /wm word mode
    dw wordMode
    dw 0
    dw error1

command_x:    
    call cmdTable
    db "or"                       ; /xor exclsuive or
    dw xor
    dw 0
    dw error1

command_default:    
    call cmdTable
    dw 0
    dw div

;                               32
div:
    pop hl                      ; hl = arg_b
    pop de                      ; de = arg_a
    inc bc
    ld a,(bc)
    cp "="
    jr z,div2
    dec bc                      ; /                    
    ex de,hl                     
div2:
    push af                     ; preserve af, bc
    push bc                         
    ld bc,hl                
    call divide
    ex de,hl
    ld (vRemain),de
    pop bc                      ; restore
    pop af
    jp sub3

; /abs absolute
; num -- num
absolute:
    pop hl
    bit 7,h
    jr z,absolute1
    xor a  
    sub l  
    ld l,a
    sbc a,a  
    sub h  
    ld h,a
absolute1:
    push hl
    jp (ix)

; /adr addrOf                   
; char -- addr
addrOf:
    pop hl                      ; a = char
    ld a,l
    call getVarAddr
    push hl
addrOf2:    
    jp (ix)

; /al length of an array, num elements
; array* -- num     
arrayLength:
    pop hl
    dec hl                      ; msb size 
    ld d,(hl)
    dec hl                      ; lsb size 
    ld e,(hl)
    ex de,hl
arrayLength1:
    push hl
    jp (ix)

; /as size in bytes of an array, based on current data width
; array* -- num     
arraySize:
    PERFORM arrayLength
    pop hl
    ld a,(vDataWidth)
    dec a
    jr z,arrayLength1
    srl h
    rr l
    jr arrayLength1

; 13
; /whi while true else break from loop             
; --
while:
    pop hl                      ; hl = condition, break if false
    ld a,l
    or h
    jr z,while1
    jp (ix)
while1:    
    ld e,iyl                    ; get block* just under stack frame
    ld d,iyh
    ld hl,8
    add hl,de
    inc hl
    inc hl
    ld (iy+2),l                 ; force first_arg* into this scope for clean up
    ld (iy+3),h                 ; first_arg* = address of block*
    jp blockEnd

; /b
byteMode:
    ld a,1
byteMode1:
    ld (vDataWidth),a
    jp (ix)

; //
comment:
    inc bc                      ; point to next char
    ld a,(bc)
    cp " "                      ; terminate on any char less than SP 
    jr nc,comment
    dec bc
    jp (ix) 

constHeapStart:
    ld de,HEAP
    jp constant

constTIBStart:
    ld de,TIB
    jp constant

constBufStart:
    ld de,BUFFER
    jp constant

decBase:
    ld a,10
decBase1:
    ld (vNumBase),a
    jp (ix)

hexBase:
    ld a,16
    jp decBase1

error1:
    ld hl,1                     ; error 1: unknown command
    jp error

; Z80 port input
; port -- value 
input:
    pop hl
    ld e,c                      ; save IP
    ld c,l
    in l,(c)
    ld h,0
    ld c,e                      ; restore IP
    push hl
    jp (ix)    

; /o Z80 port output               
; value port --
output:
    pop hl
    ld e,c                      ; save IP
    ld c,l
    pop hl
    out (c),l
    ld c,e                      ; restore IP
    jp (ix)    

; /qit
; bool -- 
quit:
    pop hl                      ; hl = condition, exit if true
    ld a,l
    or h
    jr nz,quit1
    jp (ix)
quit1:    
    jp blockEnd

recur:
    pop hl
    ld (vRecurPtr),hl
    jp (ix)

remain:
    ld hl,(vRemain)
    push hl
    jp (ix)

; select case from an associative array of cases
; bool cases* --  
select:
    pop hl                      ; hl = case associative array [ key1 value1 ... ]
    pop de                      ; de = select key
    push bc                     ; save IP
    dec hl                      ; bc = array length
    ld b,(hl)   
    dec hl
    ld c,(hl)
    inc hl
    inc hl
    jr select2
select1:
    ld a,(hl)                   ; compare lsb case key with lsb select key, hl++
    cp e
    inc hl                      ; hl++, flags are unaltered
    jr nz,select1a
    ld a,(hl)                   ; compare msb case key with msb select key, hl++
    cp d
    inc hl                      ; hl++, flags are unaltered
    jr nz,select1b
    ld e,(hl)
    inc hl
    ld d,(hl)
    pop bc
    jp go1
select1a:
    inc hl
select1b:
    inc hl
    inc hl
    dec bc
select2:
    ld a,c
    or b
    jr nz,select1
    pop bc
    jp (ix)

stringBegin:
    ld hl,TRUE                  ; string mode = true
    ld (vStrMode),hl
    jr stringEnd1              ; save hl in vBufPtr

stringEnd:
    ld hl,FALSE                 ; string mode = false
    ld (vStrMode),hl
    ld hl,(vBufPtr)             ; append NUL to string
    xor a
    ld (hl),a
    inc hl                      ; hl = string_end*
    ld (vTemp1),bc              ; save IP
    ld de,BUFFER                ; de = string* 
    or a                        ; bc = size
    sbc hl,de
    ld bc,hl
    ld hl,(vHeapPtr)            ; hl = hstring*            
    ex de,hl                    ; hl = string*, de = hstring*, bc = size
    push de                     ; return hstring*
    ldir                        ; copy size bytes from string* to hstring*
    ld (vHeapPtr),de            ; bump heap to hstring* += size
    ld bc,(vTemp1)              ; restore IP
stringEnd1:
    ld hl,BUFFER                ; reset vBufPtr
    ld (vBufPtr),hl              
    jp (ix)

stringLength:
    pop de
    ld hl,0
    jr stringLength2
stringLength1:
    inc de
    inc hl
stringLength2:
    ld a,(de)
    or a
    jr nz,stringLength1
stringLength3:
    push hl
    jp (ix)

stringSize:
    PERFORM stringLength
    pop hl
    inc hl
    jp stringLength3

; /sc string compare
; string1* string2* -- bool
; Compares two null terminated strings.
stringCompare:
    pop de
    pop hl
stringCompare1:
    ld a,(de)
    cp (hl)
    jr nz,stringCompare2
    or a
    jr z,stringCompare3
    inc de
    inc hl
    jr stringCompare1
stringCompare2:
    ld hl,FALSE
    jr stringCompare4
stringCompare3:
    ld hl,TRUE
stringCompare4:
    push hl
    jp (ix)

varBufPtr:
    ld hl,vBufPtr
    jp variable

varEcho:
    ld hl,vEcho
    jp variable

varHeapPtr:
    ld hl,vHeapPtr
    jp variable

varTIBPtr:
    ld hl,vTIBPtr
    jp variable

varHexPrefix:
    ld hl,vHexPrefix
    jp variable

; /wm
wordMode:
    ld a,2
    jp byteMode1

; /xor
xor:
    pop de                      ; Bitwise xor the top 2 elements of the stack
xor1:
    pop hl
    ld a,e
    xor l
    ld l,a
    ld a,d
    xor h
    ld h,a        
    jp add3    

;*******************************************************************
; Monty implementations
;*******************************************************************

; /rng rangeSrc
; begin end step -- src
FUNC rangeSrc, 1, "besL"            ; range source: begin, end, step, local: L                 
db "{"                              ; init mutable L [index active inrange_test]                           
db    "[%b /tru %s0>{{%a%e<}}{{%a%e>}}?] %L= " 
db    "\\kt{"                            
db      "0%t!=/qit"                  ; break if type != 0 
db      "\\dt:a{"                   ; return talkback to receive data
db        "%L1;!/qit"                ; if not active don't send
db        "%L0; %a="                ; store current index in A 
db        "%s %L0; +="              ; inc value of index by step
db        "1%t!=/qit"                ; break if type != 0
db        "%L2;^"                   ; ifte: inrange_test?
db          "{%a 1}{/fal %L1;= 0 2}"  ; ifte: /tru index, /fal active = false, quit
db          "? %k/rc"              ; ifte: send to sink note: /rc recur      
db      "} 0 %k^"                   ; init sink
db    "}" 
db "}" 
db 0

; /ai arrayIter
; array* -- src
FUNC arrayIter, 1, "aL"                             
db "{"
db    "[0 /tru %a/al] %L="            ; init mutable L [index active size]                           
db    "\\kt{"                            
db      "0%t!=/qit"                  ; break if type != 0 
db      "\\dt:i{"                   ; return talkback to receive data
db        "%L1;!/qit"                ; if not active don't send
db        "%L0; %i="                ; store current index in i 
db        "%L0; ++"                 ; inc value of index
db        "1%t!=/qit"                ; break if type != 0
db        "%i %L2; <"               ; ifte: index < size
db          "{%a%i; 1}{/fal %L1;= 0 2}"  ; ifte: /tru value, /fal active = false, quit
db          "? %k/rc"              ; ifte: send to sink note: /rc recur      
db      "} 0 %k^"                   ; init sink
db    "}" 
db "}" 
db 0

; /si stringIter
; string* -- src
FUNC stringIter, 1, "sL"                            
db "{"
db    "[0 /tru] %L="                  ; init mutable L [index active]                           
db    "\\kt{"                            
db      "0%t!=/qit"                  ; break if type != 0 
db      "\\dt:ic{"                  ; return talkback to receive data
db        "%L1;!/qit"                ; if not active don't send
db        "%L0; %i="                ; store current index in A 
db        "%L0; ++"                 ; inc value of index by step
db        "/byt %s%i; /wm %c="       ; read byte at i, store in c as word
db        "1%t!=/qit"                ; break if type != 0
db        "%c 0 !="                 ; ifte: c != NUL ?
db          "{%c 1}{/fal %L1;= 0 2}"  ; ifte: 1: send c, 2: active = false, send quit
db          "? %k/rc"              ; ifte: call sink note: /rc recur      
db      "} 0 %k^"                   ; init sink
db    "}" 
db "}" 
db 0


; /mp map
; src func -- src1
FUNC map, 0, "sf"                   ; map: source, function                 
db "{"
db    "\\kt{"                        
db      "0%t!=/qit"                  ; break if type != 0  
db      "\\dt{"                     ; call source with tb
db        "1%t=="                   ; ifte: type == 1 ?
db        "{%d %f^}{%d}"            ; ifte: func(data) or data
db        "? %t %k^"               ; ifte: send to sink
db      "} 0 %s^" 
db    "}" 
db "}" 
db 0

; /ft filter
; src pred -- src1
FUNC filter, 1, "spT"               ; filter: source, predicate, local: T  
db "{"
db    "[0]%T="
db    "\\kt{"                       ; return talkback to receive data 
db      "\\dt{"                     ; call source with tb
db        "["
db          "{%d %T0;= /tru}"         ; case 0: store talkback in T[0], return true
db          "{%d %p^}"              ; case 1: return boolean based on predicate
db          "{/tru}"                  ; case 2: return true
db        "]%t;^"                   ; select on %t
db        "{%d %t %k^}{0 1 %T0;^}"  ; ifte: true send d to sink, false send 1 to talkback
db        "?"
db      "} 0 %s^"                    
db    "}" 
db "}" 
db 0

; /fd fold
; reducer is a function like: \\da{...}
; src init reducer -- src1
FUNC fold, 1, "sirA"                    ; src, init, reducer                      
db "{"                                  ; reducer: \\da{...}
db    "[%i]%A="
db    "\\kt{"                           ; return talkback to receive data 
db      "\\dt{"                         ; call source with tb
db        "1%t=="                       ; ifte: type == 1 ?
db        "{%d %A0; %r^%A0;= %A0;}{%d}" ; ifte: reduce -> acc, acc or data 
db        "? %t %k^"                   ; ifte: send to sink
db      "} 0 %s^"                    
db    "}" 
db "}" 
db 0

; /for forEach
; src proc --
FUNC forEach, 1, "spT"              ; forEach: source, procedure, local: T                          
db "{"
db    "[0]%T="
db    "\\dt{"                       ; return talkback to receive data ; $56AA
db      "2%t==/qit"                    ; if type == 2 skip
db      "0%t=="                   ; ifte: type = 0 ?
db      "{%d %T0;=}{%d %p^}"      ; ifte: 0: store talkback, 1: send data
db      "?"                      ; ifte:
db      "0 1 %T0;^"               ; 0 or 1: get next src data item
db    "} 0 %s^" 
db "}" 
db 0

; ; /fs funcSrc
; ; func -- src
FUNC funcSrc, 0, "f"                      ; :f func or block                 
db "{"
db    "\\kt{"                              ; :kt sink, type 
db         "0%t==/whi"                     ; break if t != 0 ; TODO replace with /qit
db         "\\dt{"
db             "1%t==/whi %f^ 1 %k^"       ; if t == 1 send data to sink TODO: replace with /qit
db         "} 0 %k^"                     ; init sink
db     "}" 
db "}" 
db 0

FUNC printArray, 2, "abc"
db "{"
db "'[ '.s %a/al%c= 0%b= (%a %b ;. %b ++ %b %c </whi)^ ']'.s"
db "}"
db 0

;*******************************************************************
; implementations continued
;*******************************************************************

comma:
    call cmdTable
    db "c",NUL                   ; .c print char
    dw readChar
    db "s",NUL                   ; .s print string
    dw readString
    dw NUL                       ; .  print number, fall through
    dw readNumber

readChar:
    call getchar
    ld h,0
    ld l,a
    push hl
    jp (ix)

readString:
    ld de,(vHeapPtr)
    push de                     ; return start of string
readString1:
    call getchar
    cp "\r"
    jr z,readString2
    ld (de),a
    inc de
    ld a,(vEcho)
    inc a
    jr nz,readString1
    call putchar
    jr readString1
readString2:
    xor a
    ld (de),a
    inc de
    ld (vHeapPtr),de
    jp (ix)

readNumber:
    push ix
    PERFORM readString
    ld hl,bc                    ; save bc, hl = string*
    ex (sp),hl
    ld (vHeapPtr),hl            ; restore heap* to before string
    ld bc,hl
    ld a,(bc)
    cp "-"
    jr z,readNumber1
    cp "$"
    jr z,readNumber2
    cp "0"
    jr nc,readNumber1
    cp "9"+1
    jr c,readNumber1
    ld hl,0
    jr readNumber3
readNumber1:
    PERFORM num
    pop hl
    jr readNumber3
readNumber2:
    PERFORM hexNum
    pop hl
readNumber3:
    pop bc
    pop ix
    push hl
    jp (ix)

; ~ bitwise invert
tilde:
invert:
    pop hl                      ; Bitwise xor the top 2 elements of the stack
    ld a,l
    cpl
    ld l,a
    ld a,h
    cpl
    ld h,a        
    jp add3    

; $ hex                         ; 22
dollar:
hexnum:        
	ld hl,0	    		        ; Clear hl to accept the number
hexnum1:
    inc bc
    ld a,(bc)		            ; Get the character which is a numeral
    bit 6,a                     ; is it uppercase alpha?
    jr z, hexnum2               ; no a decimal
    sub 7                       ; sub 7  to make $a - $F
hexnum2:
    sub $30                     ; form decimal digit
    jp c,num2
    cp $0F+1
    jp nc,num2
    add hl,hl                   ; 2X ; Multiply digit(s) in hl by 16
    add hl,hl                   ; 4X
    add hl,hl                   ; 8X
    add hl,hl                   ; 16X     
    add a,l                     ; add into bottom of hl
    ld  l,a        
    jr  hexnum1

; ? if                            23
; condition then else -- value
question:
if:
    pop de                      ; de = else
    pop hl                      ; hl = then
    ex (sp),hl                  ; hl = condition, (sp) = then
    ld a,h
    or l
    pop hl                      ; hl = then
    jp z,go1                    ; if z de = else                   
    ex de,hl                    ; condition = false, de = then  
    jp go1

; string                        ;38
; -- ptr                        ; points to start of string chars,                                 ; length is stored at start - 2 bytes 
quote:
string:     
    ld hl,(vHeapPtr)            ; hl = heap*
    push hl                     ; save start of string 
    ld a,(bc)
    ld e,a                      ; e = matching terminator
    inc bc                      ; point to next char
    jr string2
string1:
    ld (hl),a
    inc hl                      ; increase count
    inc bc                      ; point to next char
string2:
    ld a,(bc)
    cp e                        ; is it the string terminator
    jr z,string3
    jr string1
string3:
    xor a                       ; write NUL to terminate string
    ld (hl),a                   ; hl = end of string
    inc hl
    ld (vHeapPtr),hl            ; bump heap* to after end of string
    jp (ix)  

; %a .. %z                      43
; -- value
; returns value of arg
percent:
arg:
    ld e,(iy+4)                 ; hl = arg_list* 
    ld d,(iy+5)
    ex de,hl                    
    ld a,l                      ; arg_list* == null, skip
    or h
    jr z,arg0a
    inc hl                      ; a = num_args, hl = arg_list*
    ld a,(hl)                    
    inc hl
    or a
    jr z,arg0a                  ; num_args == 0, skip 
    ld e,a                      ; e = a = num_args
    inc bc                      ; a = next char = dollar_name
    ld a,(bc)
    push bc                     ; save IP                         
    ld b,e                      ; b = e = num_args
    ld e,(iy+2)                 ; de = first_arg*, hl = argslist*   
    ld d,(iy+3)
arg0:
    dec de                      ; a = dollar_name, de = next arg*
    dec de
    cp (hl)
    jr z,arg1
    inc hl                      ; hl = next arg_list*            
    djnz arg0
    pop bc                      ; no match, restore IP
arg0a:
    ld de,0                     ; return 0
    jr arg1a
arg1:
    pop bc                      ; restore IP
    ex de,hl                    ; hl = arg*
    ld (vPointer),hl            ; store arg* in setter    
    ld e,(hl)
    inc hl
    ld d,(hl)                   ; de = arg
arg1a:
    push de                     ; push arg
    jp (ix)

; 0..9 number                   37
num:
	ld hl,$0000				    ; Clear hl to accept the number
	ld a,(bc)				    ; Get numeral or -
    cp '-'
    jr nz,num0
    inc bc                      ; move to next char, no flags affected
num0:
    ex af,af'                   ; save zero flag = 0 for later
num1:
    ld a,(bc)                   ; read digit    
    sub "0"                     ; less than 0?
    jr c, num2                  ; not a digit, exit loop 
    cp 10                       ; greater that 9?
    jr nc, num2                 ; not a digit, exit loop
    inc bc                      ; inc IP
    ld de,hl                    ; multiply hl * 10
    add hl,hl    
    add hl,hl    
    add hl,de    
    add hl,hl    
    add a,l                     ; add digit in a to hl
    ld l,a
    ld a,0
    adc a,h
    ld h,a
    jr num1 
num2:
    dec bc
    ex af,af'                   ; restore zero flag
    jr nz, num3
    ex de,hl                    ; negate the value of hl
    ld hl,0
    or a                        ; jump to sub2
    sbc hl,de    
num3:
    push hl                     ; Put the number on the stack
    jp (ix)                     ; and process the next character

; }                               58
rbrace:
blockEnd:
    ld e,(iy+0)                 ; vTemp1 = oldBP               
    ld d,(iy+1)
    ld (vTemp1),de
    ld e,(iy+6)                 ; vTemp2 = oldIP 
    ld d,(iy+7)
    ld (vTemp2),de
    ld e,(iy+2)                 ; hl = first_arg*, is it in this scope?
    ld d,(iy+3)
    ex de,hl                                                              
    ld e,(iy+0)                 ; de = oldBP
    ld d,(iy+1)
    inc de                      ; for carry flag <=
    or a
    sbc hl,de
    jr c,blockEnd1              ; oldBP >= first_arg, same scope skip
    ld d,iyh                    ; de = BP = first_result*, no args in this scope
    ld e,iyl
    ld hl,8
    add hl,de                   ; de = BP = first_result* (BP), hl = first_arg* (BP+8)
    ex de,hl                    ; de = first_arg*, hl = first_result*
    jr blockEnd2
blockEnd1:                      ; same scope
    ld e,(iy+2)                 ; hl = first_arg*, in scope
    ld d,(iy+3)
    ex de,hl                                                              
    ld d,iyh                    ; de = first_arg*, hl = BP = first_result*
    ld e,iyl
    ex de,hl                                                              
blockEnd2:                      
    ld bc,hl                    ; bc = hl = BP
    or a                        ; hl = BP - SP = count 
    sbc hl,sp                   
    ld a,l
    or h
    jr z,blockEnd3                      
    push bc                     ; bc = count, hl = BP
    ld bc,hl
    pop hl                      
    dec hl                      ; hl = BP-1
    dec de                      ; de = args*-1
    lddr
    inc de                      
blockEnd3:                      
    ex de,hl                    ; sp = de = new tos*
    ld sp,hl                    
    ld bc,(vTemp2)
    ld iy,(vTemp1)
    ld de,(vRecurPtr)              ; de = recur vector              
    ld a,e                      ; check for NUL
    or d
    jr nz,blockEnd4
    jp (ix)    
blockEnd4:
    ld hl,0                     ; clear recur vector
    ld (vRecurPtr),hl
    jp go1                      ; execute de
    
goBlock:
    ld (vTemp1),de              ; save de
    ld hl,stack                 ; de = BP, hl = stack, (sp) = code*
    ld d,iyh                    
    ld e,iyl
    or a                        ; if stack* == BP then this is the root_scope
    sbc hl,de                   
    ld de,(vTemp1)              ; restore de
    ld a,l                      ; if (not root_scope) then inherit scope vars from parent
    or h                    
    ld a,0
    jr z,goFunc8
    push bc                     ; push IP
    ld c,(iy+4)                 ; push arg_list* (parent)
    ld b,(iy+5)                 
    ld l,(iy+2)                 ; push first_arg* (parent)
    ld h,(iy+3)                 
goBlock2:
    push bc                     ; arg_list*
    push hl                     ; first_arg*
    push iy                     ; push BP
    ld iy,0                     ; BP = SP
    add iy,sp
goBlock3:
    ld bc,de                    ; bc = de = block*-1
    jp (ix)    

goFunc:				            ; execute func
    ex de,hl                    ; hl = func*
    ld e,(hl)                   ; de = partial_array*
    inc hl
    ld d,(hl)
    inc hl
    ld a,e                      ; if partial_array* == null skip
    or d
    jr z,goFunc3
    ld (vTemp1),bc
    ld (vTemp2),hl              ; save bc,hl
    ex de,hl                    ; hl = partial_array*
    dec hl                      ; bc = count
    ld b,(hl)
    dec hl
    ld c,(hl)
    inc hl                      ; hl = array data*
    inc hl
    jr goFunc2                  ; push each item on stack
goFunc1:
    ld e,(hl)                   ; de = partial item
    inc hl
    ld d,(hl)
    inc hl
    push de                     ; push on stack
    dec bc
goFunc2:
    ld a,c                      ; if count != 0 then loop
    or b
    jr nz,goFunc1
    ld bc,(vTemp1)              ; restore bc
    ld hl,(vTemp2)              ; restore hl
goFunc3:
    ld e,(hl)                   ; de = block*
    inc hl
    ld d,(hl)
    inc hl
    ld (vTemp1),de              ; save block*
    ld e,(hl)                   ; de = arg_list*
    inc hl
    ld d,(hl)
    inc hl
    ex de,hl                    ; hl = arg_list*
    ld de,(vTemp1)              ; restore de = block*
    ld a,l                      ; if arg_list* == null a = 0
    or h
    jr nz,goFunc4          
    xor a                       ; a = num_args (zero), num_locals (zero)
    jr goFunc8                  
goFunc4:                        ; allocate locals 
    ld a,(hl)                   ; a = num_locals*, de = block* hl = arg_list*
    jr goFunc6
goFunc5:                        ; loop
    dec sp
    dec sp
    dec a
goFunc6:
    or a
    jr nz,goFunc5               ; end loop
goFunc7:
    inc hl                      ; a = num_args* x 2 
    ld a,(hl)
    dec hl
    add a,a                     ; a *= 2
goFunc8:
    push bc                     ; push IP, a = num_args* x 2, de = block*
    ld bc,hl
    ld hl,2                     ; hl = first_arg* (BP+8), a = num args offset
    add a,l                     
    ld l,a
    add hl,sp
    jr goBlock2

; =                              21
; value _oldValue --            ; uses address in vPointer 15
assign:
    pop hl                      ; discard last accessed value
    pop hl                      ; hl = new value
assign0:
    ex de,hl                    ; de = new value
    ld hl,(vPointer)     
assign1:                        ; entry point from defineEnd
    ld (hl),e           
    ld a,(vDataWidth)                   
    dec a                       ; is it byte?
    jr z,assign2
    inc hl    
    ld (hl),d
assign2:	  
    jp (ix)  

; hl = value1, de = value2
; hl = result
equals:
    or a                        ; reset the carry flag
    sbc hl,de                   ; only equality sets hl=0 here
    jr z, true1
    jr false1

notequals:
    or a                        ; reset the carry flag
    sbc hl,de                   
    jr nz, true1
    jr false1

; hl = value1 de = value2
; hl = result
lessthaneq:    
    or a                        
    sbc hl,de    
    jr lessthan1

; hl = value1 de = value2
; hl = result
lessthan:
    or a                        
    sbc hl,de    
    jr z,false1    

lessthan1:
    jp m,false1

true1:
    ld hl, TRUE
    push hl
    jp (ix) 
null1:
false1:
    ld hl, FALSE
    push hl
    jp (ix) 

spread:
    pop hl                      ; hl = array*
    ld (vTemp1),bc              ; save bc
    dec hl                      ; bc = length
    ld b,(hl)
    dec hl
    ld c,(hl)
    inc hl                      ; move back to array 0
    inc hl
    jr spread3
spread1:    
    ld e,(hl)                   ; e = lsb data at hl
    inc hl
    ld a,(vDataWidth)           ; data width = 1, d = 0, skip     
    ld d,a
    dec d                       
    jr z,spread2
    ld d,(hl)                   ; d = msb data at hl
    inc hl
spread2:
    push de                     ; return de
    dec bc                      ; count--
spread3:
    ld a,c                      ; exit loop if bc == 0
    or b
    jr nz,spread1
    ld bc,(vTemp1)              ; restore bc
    jp (ix)

; shiftLeft                     
; value count <<           
; count variable <<=           
shiftLeft:
    pop hl                      ; de = arg_a, hl = arg_b
    pop de                      
    inc bc
    ld a,(bc)
    cp "="
    jr z,shiftLeft2
    dec bc                                          
    ex de,hl                    ; de = arg_b, hl = arg_a 
shiftLeft2:
    ld (vTemp1),bc              ; save IP
    ld b,e                      ; b = loop counter
    inc b                       ; test for counter=0 case
    jr shiftLeft4
shiftLeft3:   
    add hl,hl                   ; left shift hl
shiftLeft4:   
    djnz shiftLeft3
    ld bc,(vTemp1)              ; restore IP
    jp sub3

; shiftRight                     
; value count >>           
; count variable >>=           
shiftRight:
    pop hl                      ; de = arg_a, hl = arg_b
    pop de                      
    inc bc
    ld a,(bc)
    cp "="
    jr z,shiftRight2
    dec bc                                          
    ex de,hl                    ; de = arg_a, hl = arg_b 
shiftRight2:
    ld (vTemp1),bc              ; save IP
    ld b,e                      ; b = loop counter
    inc b                       ; test for counter=0 case
    jr shiftRight4
shiftRight3:   
    srl h                       ; right shift hl
    rr l
shiftRight4:   
    djnz shiftRight3
    ld bc,(vTemp1)              ; restore IP
    jp sub3


; division subroutine.
; bc: divisor, de: dividend, hl: remainder

divide:        
    ld hl,0    	                ; zero the remainder
    ld a,16    	                ; loop counter
divide1:		                ; shift the bits from bc (numerator) into hl (accumulator)
    sla c
    rl b
    adc hl,hl
    sbc hl,de		            ; check if remainder >= denominator (hl>=de)
    jr c,divide2
    inc c
    jr divide3
divide2:		                ; remainder is not >= denominator, so we have to add de back to hl
    add hl,de
divide3:
    dec a
    jr nz,divide1
    ld de,bc                    ; result from bc to de
    ret

dotNext:
    ld a,(vStrMode)             ; if string mode then exit
    inc a                       
    jr nz,dotNext1
    jp (ix)
dotNext1:
    ld de,BUFFER
    ld hl,(vBufPtr)
    ld (hl),0                   ; store NUL at end of string
    ld (vBufPtr),de             ; reset vBufPtr to vHeapPtr
    ex de,hl                    ; hl = BUFFER
    call putstr
    jp (ix)

; arg_list - parses arg_list e.g. ab:c
; -- arg_list* 
parseArgs:
    ld de,0                     ; d = count locals, e = count args ()
    ld hl,(vHeapPtr)            ; hl = heap*
    push hl                     ; save start of arg_list
    inc hl                      ; skip length fields to start of string
    inc hl
    inc bc                      ; point to next char
parseArgs1:
    ld a,(bc)
    cp ":"                      ; ":" switches from args to locals
    jr nz,parseArgs1a
    inc d                       ; non zero value local count acts as flag
    jr parseArgs3
parseArgs1a:
    cp "a"                      ; < "a" terminates arg_list
    jr c,parseArgs4
    cp "z"+1                    ; > "z" terminates arg_list
    jr nc,parseArgs4
parseArgs2:
    ld (hl),a
    inc hl                      
    inc e                       ; increase arg count
    xor a
    or d
    jr z,parseArgs3
    inc d                       ; if d > 0 increase local count
parseArgs3:
    inc bc                      ; point to next char
    jr parseArgs1
parseArgs4:
    dec bc
    xor a
    or d
    jr z,parseArgs5
    dec d                       ; remove initial inc
parseArgs5:
    inc hl
    ld (vHeapPtr),hl            ; bump heap* to after end of string
    pop hl                      ; hl = start of arg_list
    ld (hl),d                   ; write number of locals at start - 1                                      
    inc hl                      
    ld (hl),e                   ; write number of args + locals at start - 2
    dec hl
    ex (sp),hl
    jp (hl)  

; create block: parses block e.g. { .... }
; -- block*
parseBlock:
    push bc                     ; return pointer to first { of block    
    inc bc
    ld d,1                      ; nesting: count first parenthesis
parseBlock1:                         ; Skip to end of definition    
    ld a,(bc)                   ; Get the next character
    inc bc                      ; Point to next character
    cp " " + 1                  ; ignore whitespace 
    jr c,parseBlock1
    cp "'"                      ; quote char
    jr z,parseBlock2
    cp DQ                       ; double quote char
    jr z,parseBlock2
    cp "`"                      ; grave char
    jr z,parseBlock2
    bit 7,d    
    jr nz,parseBlock1    
    jp parseBlock3
parseBlock2:
    ld a,$80
    xor d
    ld d,a
    jr parseBlock1    
parseBlock3:
    cp "("
    jr z,parseBlock4
    cp "{"
    jr z,parseBlock4
    cp "["
    jr z,parseBlock4
    cp ")"
    jr z,parseBlock5
    cp "}"                       
    jr z,parseBlock5
    cp "]"
    jr z,parseBlock5
    jr parseBlock1
parseBlock4:
    inc d
    jr parseBlock1                   
parseBlock5:
    dec d
    jr nz, parseBlock1          ; get the next element
    ld hl,bc                    ; hl = IP
    ld de,HEAP                  ; is IP pointing to object in heap
    or a                        ; IP - HEAP
    sbc hl,de
    bit 7,h                     ; if -ve then copy to heap else skip
    jr z,parseBlock6
    ld hl,bc                    ; hl = IP
    pop de                      ; de = block*
    ld (vTemp1),bc              ; save IP
    or a                        ; bc = size
    sbc hl,de
    ld bc,hl
    ex de,hl                    ; hl = block* de = heap*
    ld de,(vHeapPtr)            
    push de                     ; return hblock*
    ldir                        ; copy size bytes from block* to hblock*
    ld (vHeapPtr),de            ; heap* += size
    ld bc,(vTemp1)              ; restore IP
parseBlock6:
    dec bc                      ; balanced, exit
    pop hl                      ; hl = block*
    ex (sp),hl                  ; return to caller
    jp (hl)  

; create function
; arg_list* block* -- func*
createFunc:
    pop hl                      ; save retrn address
    ld (vTemp3),hl
    ld (vTemp1),bc              ; save IP
    pop hl                      ; hl = block*
    ld (vTemp2),hl              ; save block*
    ld e,(iy+4)                 ; de = outer_arg_list 
    ld d,(iy+5)
    ld a,e                      ; if arg_list == null then make a func
    or d
    jr nz,createFunc0
    ld hl,0                     ; partial_array = null
    ld de,(vHeapPtr)            ; de = compile*
    jr createFunc5                 
createFunc0:
    pop hl                      ; hl = inner_arg_list*
    push hl                     ; save inner_arg_list
    ld de,(vHeapPtr)            ; de = compile*
    ld a,(hl)                   ; compile inner_num_locals
    ld c,a                      ; b = inner_num_locals
    ld (de),a
    inc hl
    inc de
    ld a,(hl)                   ; compile inner_length
    ld (de),a
    sub c                       ; a = inner_num args
    inc hl
    inc de
    or a                        ; compile args if inner_length > 0
    jr z,createFunc1
    ld c,a                      ; bc = a
    ld b,0
    ldir
createFunc1:    
    ex de,hl                    ; hl = outer_arg_list
    ld e,(iy+4)                  
    ld d,(iy+5)
    ex de,hl
    inc hl                      ; a = outer_length
    ld a,(hl)
    inc hl      
    or a
    jr z,createFunc2
    ld c,a
    ld b,0
    ldir                        ; append outer_args
createFunc2:                      ; a = outer_length 
    ld b,a                      ; b = a = outer_length
    ld hl,(vHeapPtr)            ; b > 0, hl = start of cloned arg_list
    inc hl
    ld a,(hl)                   ; add outer_length to new length
    add a,b                     
    ld (hl),a
    dec hl
    ld a,b                      ; save outer_length in a'
    ex af,af'                   
    ex (sp),hl                  ; hl = inner_arg_list*, (sp) new_arg_list                      
    ld a,(hl)                   ; c = a = inner_num_locals
    or a
    jr z,createFunc2a             ; if inner_num_locals == 0 skip
    ld c,a                      ; c = inner_num_locals
    ld b,0                      ; bc = inner_num_locals
    inc hl                      ; a = inner_length
    ld a,(hl)                    
    sub c                       ; a = inner_num_args
    inc hl                      ; hl = inner_arg_chars
    add a,l                     ; hl += a
    ld l,a
    ld a,0
    add a,h
    ld h,a
    ldir                        ; append inner_locals
createFunc2a:    
    ex af,af'                   ; restore outer_length to a, de = partial_array[-2]
    ld (de),a                   ; compile partial_array length field 
    inc de
    xor a
    ld (de),a
    inc de
    push de                     ; push partial_array*
    ex de,hl                    ; hl = first_arg, copy outer_args+locals to partial_array
    ld e,(iy+2)                     
    ld d,(iy+3)
    ex de,hl
createFunc3:
    dec hl                      ; c = MSB of arg from stack (incl. locals)
    ld c,(hl)
    dec hl
    ld a,(hl)                   ; a = LSB of arg from stack (incl. locals)
    ld (de),a                   ; write LSB and MSB to partial_array*
    inc de
    ld a,c
    ld (de),a
    inc de
    djnz createFunc3              ; b = outer_length
createFunc4:
    pop hl                      ; hl = partial_array*
createFunc5:
    pop bc                      ; bc = new_arg_list*
    push de                     ; return new func*
    ex de,hl                    ; hl = new func*, de = partial_array*
    ld (hl),e                   ; compile partial_array* to func
    inc hl                       
    ld (hl),d
    inc hl
    ld de,(vTemp2)              ; de = block*
    ld (hl),e                   ; compile block* to func
    inc hl
    ld (hl),d
    inc hl
    ld (hl),c                   ; compile new_arg_list* to func
    inc hl
    ld (hl),b
    inc hl
    ld (vHeapPtr),hl            ; bump heap ptr
    ld bc,(vTemp1)              ; restore IP
    ld hl,(vTemp3)              ; jump to return address
    jp (hl)

;*******************************************************************
; general routines
;*******************************************************************

; hl = address
variable:
    ld (vPointer),hl
    ld e,(hl)
    ld a,(vDataWidth)
    dec a
    ld d,0
    jr z,constant
    inc hl
    ld d,(hl)
    dec hl
constant:
    push de
    jp (ix)

; followed by a table
; indexed on the 0-25 lowercase letter
; db lsb(addr)
; the final item index 26 matches any other char
charTable:
    inc bc
    ld a,(bc)
    cp "z"+1
    jr nc,charTable3
    sub "a" 
    jr c,charTable3
charTable1:
    pop hl
    add a,l
    ld l,a
    ld a,(hl)                   ; must have the same msb as the table
    or a                        ; a = 0, nop
    jr nz,charTable2
    jp error1
charTable2:
    ld l,a
    jp (hl)
charTable3:
    ld a,26
    dec bc
    jr charTable1
    
; followed by a table
; db char
; db char - if null only match on first char
; dw addr
; the final item must have char == NUL
cmdTable:
    pop hl
cmdTable1:
cmdTable2:
    ld d,(hl)
    inc hl
    ld e,(hl)                   
    inc hl
    xor a                       ; if d == 0, matched
    cp d
    jr z,cmdTable5
    inc bc                      ; match? 
    ld a,(bc)
    cp d
    jr nz,cmdTable4
cmdTable3:
    xor a                       ; if e == 0, matched 
    cp e
    jr z,cmdTable5
    inc bc
    ld a,(bc)                   ; match? 
    cp e
    jr z,cmdTable5
cmdTable4:                      ; no match, restore bc, go to next table entry
    dec bc
    inc hl
    inc hl
    jr cmdTable2
cmdTable5:                      ; matched, jump to addr
    ld e,(hl)                   
    inc hl
    ld d,(hl)
    ex de,hl
    jp (hl)

getVarAddr:
    ld hl,0
    cp "z"+1                    ; if a > z then exit
    ret nc
    sub "A"                     ; a - 65
    ret c                       ; if < A then exit
    cp "Z"+1-"A"                ; if > Z then subtract 7
    jr c,getVarAddr1
    sub "a"-("Z"+1)
    cp "Z"-"A"+1
    ret c                       ; if < a then exit
getVarAddr1:
    add a,a                     ; double a
    ld l,a
    ld h,msb(VARS)     
    ret

putstr0:
    call putchar
    inc hl
putstr:
    ld a,(hl)
    or a
    jr nz,putstr0
    ret

; **************************************************************************    
; calculate nesting value
; a is char to be tested, 
; e is the nesting value (initially 0)
; e is increased by ( and [ 
; e is decreased by ) and ]
; e has its bit 7 toggled by `
; limited to 127 levels
; **************************************************************************    

nesting:    
    cp "'"                      ; quote char
    jr z,nesting0
    cp DQ                       ; double quote char
    jr z,nesting0
    cp "`"                      ; grave char
    jr z,nesting0
    jr nesting1
nesting0:
    bit 7,e
    jr z,nesting1a
    res 7,e
    ret
nesting1a: 
    set 7,e
    ret
nesting1:
    bit 7,e    
    ret nz    
    cp '{'
    jr z,nesting2
    cp '['
    jr z,nesting2
    cp '('
    jr nz,nesting3
nesting2:
    inc e
    ret
nesting3:
    cp '}'
    jr z,nesting4
    cp ']'
    jr z,nesting4
    cp ')'
    ret nz
nesting4:
    dec e
    ret 
 
prompt:          
    call printStr
    .cstr "\r\n> "
    ret

crlf:       
    call printStr
    .cstr "\r\n"
    ret

; prints a null teminated string
; the string should be immediately following the call
printStr:        
    ex (sp),hl		            ; swap			
    call putstr		
    inc hl			            ; inc past NUL
    ex (sp),hl		            ; put it back	
    ret

init:
    ld hl,titleStr
    ld de,titleBuf
    ld b,20
init1:
    ld a,(de)
    cp (hl)
    jr nz,coldBoot0
    inc de
    inc hl
    djnz init1

warmInit:
    ld bc,(vSavedIP)            ; restore IP
    ld sp,(vSavedSP)            ; restore SP
    ld ix,(vSavedNext)          ; restore Next
    ld iy,(vSavedBP)            ; restore BP
    jp start1

coldBoot0:    
    ld hl,titleStr              ; copy titleStr to titleBuf
    ld de,titleBuf
    ld b,20
    ldir

coldInit:    
    ld hl,isysVars
    ld de,sysVars
    ld bc,4 * 2 + 10
    ldir

    ld hl,vars                  ; 52 vars LO HI 
    ld b,26*2                       
    xor a
coldBoot1:
    ld (hl),a
    inc hl
    djnz coldBoot1

    ld ix,NEXT
    ld iy,STACK
    ret

coldStart:
    ld sp,STACK
    call coldBoot0
    jp start1
start:
    ld sp,STACK		            ; start Monty
    call init		            ; setups
start1:
    ld hl,titleBuf
    call putstr 		        ; prog count to stack, put code line 235 on stack then call print

interpret:

    call prompt

    ld bc,0                     ; load TIB length, decide char into tib or execute or control    
    ld hl,TIB
    ld (vTIBPtr),hl             ; no chars in TIB so set end pointer to beginning           

interpret2:                     ; calculate nesting 

    ld e,0                      ; initilize nesting value
    push bc                     ; save offset into TIB, 
                                ; bc is also the count of chars in TIB
    ld hl,TIB                   ; hl is start of TIB
    jr interpret4

interpret3:
    ld a,(hl)                   ; a = char in TIB
    inc hl                      ; inc pointer into TIB
    dec bc                      ; dec count of chars in TIB
    call nesting                ; update nesting value

interpret4:

    ld a,c                      ; is count zero?
    or b
    jr nz, interpret3           ; if not loop
    pop bc                      ; restore offset into TIB
    
interpret5:    

    call getchar                ; loop around waiting for character from serial port
    cp $20			            ; compare to space
    jr nc,interpret6		        ; if >= space, if below 20 set cary flag
    cp NUL                      ; is it end of string? NUL end of string
    jr z,interpret8
    cp '\r'                     ; carriage return? ascii 13
    jr z,interpret7		        ; if anything else its macro/control 

    cp CTRL_H
    jp z,backSpace_
    cp CTRL_J
    jp z,reEdit_

interpret5a:    
    jr interpret2

interpret6:

    ld hl,TIB
    add hl,bc
    ld (hl),a                   ; store the character in textbuf
    inc bc
    call putchar                ; echo character to screen
    call nesting
    jr  interpret5                ; wait for next character

interpret7:

    ld hl,TIB
    add hl,bc
    ld (hl),"\r"                ; store the crlf in textbuf
    inc hl
    ld (hl),"\n"  
    inc hl    
    inc bc
    inc bc
    call crlf                   ; echo character to screen
    ld a,e                      ; if zero nesting append and ETX after \r
    or a
    jr nz,interpret5

interpret8:

    ld hl,TIB
    add hl,bc
    ld (vTIBPtr),hl
    ld bc,TIB                   

    ld (vSavedIP),bc            ; save IP
    ld (vSavedSP),sp            ; save SP
    ld (vSavedNext),ix          ; save Next
    ld (vSavedBP),iy            ; save BP
                                
    dec bc
NEXT:        
    inc bc                      ; Increment the IP
    ld a,(bc)                   ; Get the next character and dispatch
    cp " "                      ; whitespace?
    jr z,next                   ; space? ignore
    jr c,next1
    add a,$80 - "!"             ; subtract "!", add $80 (opcodes lsb starts at $80)
    ld l,a                      ; index into table
    ld h,msb(opcodes)           ; start address of jump table    
    ld l,(hl)                   ; get low jump address
    inc h                       ; page 2
    cp "Z" - "!" + 1 + $80
    jr c,next0
    inc h                       ; page 3
next0:
    jp (hl)                     ; Jump to routine
next1:
    cp NUL                      ; end of input string?
    jr z,exit
    jp interpret                ; no, other whitespace, macros?

exit:
    inc bc
    ld hl,bc
    jp (hl)

run:
    pop bc
    dec bc
    jp (ix)

error:
    push hl
    call run
    db "`Error ` .",0
    jp interpret

backSpace_:
    ld a,c
    or b
    jp z, interpret2
    dec bc
    call printStr
    .cstr "\b \b"
    jp interpret2

reEdit_:
    call printStr
    .cstr "\r> "
    ld hl,TIB
    jr reEdit1
reEdit0:
    call putchar
    inc hl
reEdit1:
    ld a,(hl)
    cp "\r"
    jr nz,reEdit0
    ld de,TIB
    or a
    sbc hl,de
    ld bc,hl
    jp interpret2
