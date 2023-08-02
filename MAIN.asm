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
    DW 2                        ; vDataWidth in bytes of array operations (default 1 byte) 
    DW 10                       ; vNumBase = 10
    DW TIB                      ; vTIBPtr pointer into TIB
    DW BUFFER                   ; vBufPtr pointer into BUF
    DW next                     ; nNext
    DW HEAP                     ; vHeapPtr \h start of the free mem
    DW 0                        ; vRecur
    DW 0                        ; vDefine
    DW 0                        ; vStrMode

; **********************************************************************			 
; title string (also used by warm boot) 
; **********************************************************************

titleStr:
    .cstr ESC,"[2JMonty V0.1\r\n",0,0,0

;********************** PAGE 2 BEGIN ***********************************

dquote_:
comma_:
    jp (ix)

dollar_:
    jp dollar

percent_:        
    jp percent 

quote_:
    jp quote

lparen_:
    jp lbrace

dot_:  
    jp dot

slash_:
    jp slash

num_:    
    jp  num

semicolon_:
    jp semicolon
  
question_:
    jp question

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
hash:
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
    ld (vPointer),hl                    ; store address in setter    
    ld d,0
    ld e,(hl)
    or a                                ; check data width again                                
    jr z,arrayIndex2
    inc hl
    ld d,(hl)
arrayIndex2:
    push de
    jp (ix)

;                               4
rparen_:
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

; - sub                          23
; a b -- c
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
    inc bc
    cp "-"
    jr nz,sub1
    pop hl
    dec hl
    jp assign0
sub1:
    dec bc
    pop de
    pop hl
    or a
    sbc hl,de    
    jr add3

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

colon_:
colon:
    inc bc                      ; arg_list must ve immediately followed by {
    ld a,(bc)
    cp "="                      ; := definition
    jr z,defineStart
    dec bc
    ld hl,1
    jp error
defineStart:
    pop hl                      ; discard variable value
    ld hl,(vPointer)            ; vDefine = vPointer
    ld (vDefine),hl
    jp (ix)

;                               18
upcase_:
upcase:
    ld a,(bc)                   ; a = identifier char
    sub 'A'                     ; 'A' = 0
    jr ident1

; ;
semicolon:
defineEnd:    
    ld hl,(vDefine)             ; hl = define*    
    ld a,l
    or h
    jr z,defineEnd1
    ld de,NUL                   ; set vDefine=NUL
    ld (vDefine),de
    pop de                      ; de = value
    jp assign1
defineEnd1:
    jp (ix)


;********************** PAGE 2 END *********************************************
; .align $100
;********************** PAGE 3 BEGIN *********************************************

lowcase_:
lowcase:
    ld a,(bc)
    sub 'a' 
    add a,26
ident1:
    add a,a                     ; l = a * 2                             
    ld l,a
    ld h,msb(vars)     
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
    call commandTable
    db "a"                      ; .a print array
    dw printArray
    db "c"                      ; .c print char
    dw printChar
    db "h"                      ; .h print hex without $ prefix
    dw printHex0
    db "s"                      ; .s print string
    dw printString
    db NUL                      ; .  print number, fall through
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

printHex0:                      
    ld de,(vBufPtr)
    jr printHex1
printHex:                      
    ld de,(vBufPtr)
    ld a,"$"                    ; # prefix
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

; unused

dquote:
underscore:
comma:
    jp (ix)

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
    call lowerCaseTable
    db lsb(command_a_)
    db lsb(command_b_)
    db lsb(command_nop_)
    db lsb(command_d_)
    db lsb(command_nop_)
    db lsb(command_f_)
    db lsb(command_nop_)
    db lsb(command_h_)
    db lsb(command_i_)
    db lsb(command_nop_)
    db lsb(key_)
    db lsb(command_nop_)
    db lsb(command_m_)
    db lsb(command_nop_)
    db lsb(output_)
    db lsb(command_p_)
    db lsb(command_q_)
    db lsb(command_r_)
    db lsb(command_s_)
    db lsb(true_)
    db lsb(command_nop_)
    db lsb(command_v_)
    db lsb(command_w_)
    db lsb(xor_)
    db lsb(command_nop_)
    db lsb(command_nop_)
    db lsb(command_default_)

; 12
command_a_:
    db "b"                      ; /ab absolute
    dw absolute
    db "d"                      ; /ad address of
    dw addrOf
    db "i"                      ; /ad address of
    dw arrayIter
    db "l"                      ; /al array length
    dw arrayLength
    db "s"                      ; /as array size
    dw arraySize
    db NUL
    dw error1

command_b_:
    db "b"                      ; /bb bye bye cold boot
    dw coldStart
    db "m"                      ; /bm byte mode
    dw byteMode
    db "r"                      ; /br break from loop
    dw break
    db NUL
    dw error1

command_d_:
    db "c"                      ; /dc decimal
    dw decBase
    db NUL
    dw decBase                  ; /d decimal

command_f_:
    db "d"                      ; /fd fold
    dw fold
    db "e"                      ; /fe forEach
    dw forEach
    db "s"                      ; /fs funcSrc
    dw funcSrc
    db "t"                      ; /ft filter
    dw filter
    db "1"                      
    dw f1
    db "2"                      
    dw f2
    db "3"                      
    dw f3
    db "4"                      
    dw f4
    db NUL
    dw false1

command_h_:
    db "b"                      ; /hx hex
    dw hexBase
    db NUL
    dw error1                   

; 6
command_i_:
    db "n"                      ; /in input
    dw input
    db NUL
    dw error1

key_:
    db NUL
    dw key

command_m_:
    db "p"                      ; /mp map
    dw map
    db NUL
    dw error1

output_:
    db NUL
    dw output
; 4
command_p_:
    db NUL
    dw error1

; 6
command_q_:
    db "t"                      ; /qt quit
    dw quit
    db NUL
    dw error1

command_r_:
    db "c"                      ; /rc tail call optimisation
    dw recur
    db "e"                      ; /re remainder
    dw remain
    db "g"                      ; /rg range src
    dw rangeSrc
    db NUL
    dw error1

command_s_:
    db "b"
    dw stringBegin
    db "c"
    dw stringCompare
    db "e"
    dw stringEnd
    db "i"
    dw stringIter
    db "l"
    dw stringLength
    db "s"
    dw stringSize
    db NUL
    dw error1

true_:
    db NUL
    dw true1

command_v_:
    db "b"
    dw varBufPtr
    db "h"
    dw varHeapPtr
    db "t"
    dw varTIBPtr
    db "B"
    dw constBufStart
    db "H"
    dw constHeapStart
    db "T"
    dw constTIBStart
    db NUL
    dw error1

command_w_:
    db "m"                      ; /wm word mode
    dw wordMode
    db NUL
    dw error1

xor_:
    db NUL
    dw xor

; 2
command_nop_:
    db NUL
    dw nop
 
; 3
command_default_:
    db NUL
    dw div

;********************** PAGE 5 END *********************************************

nop:
    jp (ix)

;                               32
div:
    pop de
    pop hl
    push bc                     ; preserve the IP    
    ld bc,hl                
    call divide
    ex de,hl
    ld (vRemain),de
    pop bc
    jp add3

; /ab absolute
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

; /ad addrOf                    24
; char -- addr
addrOf:
    pop hl                      ; a = char
    ld a,l
    cp "z"+1                    ; if a > z then exit
    jr nc,addrOf2
    sub "A"                     ; a - 65
    jr c,addrOf2                ; if < A then exit
    cp "Z"+1-"A"                ; if > Z then subtract 7
    jr c,addrOf1
    sub "a"-("Z"+1)
    cp "Z"-"A"+1
    jr c,addrOf2                ; if < a then exit
addrOf1:
    add a,a                     ; double a
    ld hl,VARS                  ; hl = VARS + a
    add a,l
    ld l,a
    ld a,0
    adc a,h
    ld h,a
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
; /br break from loop             
; --
break:
    pop hl                      ; hl = condition, break if false
    ld a,l
    or h
    jr z,break1
    jp (ix)
break1:    
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
    ld hl,1
byteMode1:
    ld (vDataWidth),hl
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
    ld hl,10
decBase1:
    ld (vNumBase),hl
    jp (ix)

error1:
    ld hl,1                     ; error 1: unknown command
    jp error

hexBase:
    ld hl,16
    jp decBase1

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

; /k                              6
key:
    call getchar
    ld h,0
    ld l,a
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

; /qt
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
    ld (vRecur),hl
    jp (ix)

remain:
    ld hl,(vRemain)
    push hl
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
    ld de,(vBufPtr)
    ld hl,vBufPtr
    jr variable

varHeapPtr:
    ld de,(vHeapPtr)
    ld hl,vHeapPtr
    jr variable

varTIBPtr:
    ld de,(vTIBPtr)
    ld hl,vTIBPtr
    jr variable

variable:
    ld (vPointer),hl
constant:
    push de
    jp (ix)

; /wm
wordMode:
    ld hl,2
    jp byteMode1

; /x
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

; /rg rangeSrc
; begin end step -- src
FUNC rangeSrc, 1, "besL"            ; range source: begin, end, step, local: L                 
db "{"                              ; init mutable L [index active inrange_test]                           
db    "[%b /t %s0>{{%a%e<}}{{%a%e>}}??] %L= " 
db    "\\kt{"                            
db      "0%t!=/qt"                  ; break if type != 0 
db      "\\dt:a{"                   ; return talkback to receive data
db        "%L1#!/qt"                ; if not active don't send
db        "%L0# %a="                ; store current index in A 
db        "%s %L0# +="              ; inc value of index by step
db        "1%t!=/qt"                ; break if type != 0
db        "%L2#^"                   ; ifte: inrange_test?
db          "{%a 1}{/f %L1#= 0 2}"  ; ifte: /t index, /f active = false, quit
db          "?? %k/rc"              ; ifte: send to sink note: /rc recur      
db      "} 0 %k^"                   ; init sink
db    "}" 
db "}" 
db 0

; /ai arrayIter
; array* -- src
FUNC arrayIter, 1, "aL"                             
db "{"
db    "[0 /t %a/al] %L="            ; init mutable L [index active size]                           
db    "\\kt{"                            
db      "0%t!=/qt"                  ; break if type != 0 
db      "\\dt:i{"                   ; return talkback to receive data
db        "%L1#!/qt"                ; if not active don't send
db        "%L0# %i="                ; store current index in i 
db        "%L0# ++"                 ; inc value of index
db        "1%t!=/qt"                ; break if type != 0
db        "%i %L2# <"               ; ifte: index < size
db          "{%a%i# 1}{/f %L1#= 0 2}"  ; ifte: /t value, /f active = false, quit
db          "?? %k/rc"              ; ifte: send to sink note: /rc recur      
db      "} 0 %k^"                   ; init sink
db    "}" 
db "}" 
db 0

; /si stringIter
; string* -- src
FUNC stringIter, 1, "sL"                            
db "{"
db    "[0 /t] %L="                  ; init mutable L [index active]                           
db    "\\kt{"                            
db      "0%t!=/qt"                  ; break if type != 0 
db      "\\dt:ic{"                  ; return talkback to receive data
db        "%L1#!/qt"                ; if not active don't send
db        "%L0# %i="                ; store current index in A 
db        "%L0# ++"                 ; inc value of index by step
db        "/bm %s%i# /wm %c="       ; read byte at i, store in c as word
db        "1%t!=/qt"                ; break if type != 0
db        "%c 0 !="                 ; ifte: c != NUL ?
db          "{%c 1}{/f %L1#= 0 2}"  ; ifte: 1: send c, 2: active = false, send quit
db          "?? %k/rc"              ; ifte: call sink note: /rc recur      
db      "} 0 %k^"                   ; init sink
db    "}" 
db "}" 
db 0


; /mp map
; src func -- src1
FUNC map, 0, "sf"                   ; map: source, function                 
db "{"
db    "\\kt{"                        
db      "0%t!=/qt"                  ; break if type != 0  
db      "\\dt{"                     ; call source with tb
db        "1%t=="                   ; ifte: type == 1 ?
db        "{%d %f^}{%d}"            ; ifte: func(data) or data
db        "?? %t %k^"               ; ifte: send to sink
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
db          "{%d %T0#= /t}"         ; case 0: store talkback in T[0], return true
db          "{%d %p^}"              ; case 1: return boolean based on predicate
db          "{/t}"                  ; case 2: return true
db        "]%t#^"                   ; select on %t
db        "{%d %t %k^}{0 1 %T0#^}"  ; ifte: true send d to sink, false send 1 to talkback
db        "??"
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
db        "{%d %A0# %r^%A0#= %A0#}{%d}" ; ifte: reduce -> acc, acc or data 
db        "?? %t %k^"                   ; ifte: send to sink
db      "} 0 %s^"                    
db    "}" 
db "}" 
db 0

; /fe forEach
; src proc --
FUNC forEach, 1, "spT"              ; forEach: source, procedure, local: T                          
db "{"
db    "[0]%T="
db    "\\dt{"                       ; return talkback to receive data ; $56AA
db      "2%t==/qt"                    ; if type == 2 skip
db      "0%t=="                   ; ifte: type = 0 ?
db      "{%d %T0#=}{%d %p^}"      ; ifte: 0: store talkback, 1: send data
db      "??"                      ; ifte:
db      "0 1 %T0#^"               ; 0 or 1: get next src data item
db    "} 0 %s^" 
db "}" 
db 0

; ; /fs funcSrc
; ; func -- src
FUNC funcSrc, 0, "f"                      ; :f func or block                 
db "{"
db    "\\kt{"                              ; :kt sink, type 
db         "0%t==/br"                     ; break if t != 0 
db         "\\dt{"
db             "1%t==/br %f^ 1 %k^"       ; if t == 1 send data to sink
db         "} 0 %k^"                     ; init sink
db     "}" 
db "}" 
db 0

FUNC printArray, 2, "abc"
db "{"
db "'[ '.s %a/al%c= 0%b= (%a %b #. %b ++ %b %c </br)^ ']'.s"
db "}"
db 0

;*******************************************************************
; implementations continued
;*******************************************************************

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
; condition then -- value
question:
if:
    inc bc
    ld a,(bc)
    cp "?"
    jr z,ifte
    dec bc
    ld de,NUL                   ; NUL pointer for else
    jr ifte1

; ?? ifte
; condition then else -- value
ifte: 
    pop de                      ; de = else
ifte1:
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
    ld de,(vRecur)              ; de = recur vector              
    ld a,e                      ; check for NUL
    or d
    jr nz,blockEnd4
    jp (ix)    
blockEnd4:
    ld hl,0                     ; clear recur vector
    ld (vRecur),hl
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

; shiftLeft                     15
; value count -- value2          shift left count places
shiftLeft:
    ld de,bc                    ; save IP    
    pop bc                      ; bc = count
    ld b,c                      ; b = loop counter
    pop hl                      
    inc b                       ; test for counter=0 case
    jr shiftLeft2
shiftLeft1:   
    add hl,hl                   ; left shift hl
shiftLeft2:   
    djnz shiftLeft1
    push hl
    ld bc,de                    ; restore IP
    jp (ix)

; shiftRight                    16
; value count -- value2          shift left count places
shiftRight:
    ld de,bc                    ; save IP    
    pop bc                      ; bc = count
    ld b,c                      ; b = loop counter
    pop hl                      
    inc b                       ; test for counter=0 case
    jr shiftRight2
shiftRight1:   
    srl h                       ; right shift hl
    rr l
shiftRight2:   
    djnz shiftRight1
    push hl
    ld bc,de                    ; restore IP
    jp (ix)

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

    cp ")"
    jr z,parseBlock4
    cp "}"                       
    jr z,parseBlock4
    cp "]"
    jr z,parseBlock4

    cp "("
    jr z,parseBlock2
    cp "{"
    jr z,parseBlock2
    cp "["
    jr z,parseBlock2

    cp "'"
    jr z,parseBlock3
    cp "`"
    jr z,parseBlock3
    cp DQ
    jr z,parseBlock3
    jr parseBlock1
parseBlock2:
    inc d
    jr parseBlock1                   
parseBlock3:
    ld a,$80
    xor d
    ld d,a
    jr nz, parseBlock1
    jr parseBlock5
parseBlock4:
    dec d
    jr nz, parseBlock1          ; get the next element
parseBlock5:
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

; followed by a table
; indexed on the 0-25 lowercase letter
; db lsb(addr)
; the final item index 26 matches any other char
lowerCaseTable:
    inc bc
    ld a,(bc)
    cp "z"+1
    jr nc,lowerCaseTable2
    sub "a" 
    jr c,lowerCaseTable2
lowerCaseTable1:
    pop hl
    add a,l
    ld l,a
    ld l,(hl)                   ; must have the same msb as the table
    jp commandTable1
lowerCaseTable2:
    ld a,26
    dec bc
    jr lowerCaseTable1
    
; followed by a table
; db char
; dw addr
; the final item must have char == NUL
commandTable:
    pop hl
commandTable1:
    inc bc
commandTable2:
    xor a
    cp (hl)
    jr nz,commandTable3
    dec bc
    jr commandTable4
commandTable3:
    ld a,(bc)
    cp (hl)
    jr z,commandTable4
    inc hl
    inc hl
    inc hl
    jr commandTable2
commandTable4:
    inc hl
    ld e,(hl)                   
    inc hl
    ld d,(hl)
    ex de,hl
    jp (hl)

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
    cp DQ                       ; quote char
    jr z,nesting0
    cp "`"                      ; quote char
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
    ld bc,9 * 2
    ldir

    ld hl,vars                  ; 52 vars LO HI 
    ld b,26*2                       
    xor a
coldBoot1:
    ld (hl),a
    inc hl
    djnz coldBoot1

    ld ix,(vNext)
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
    ld a,"3"
    call putchar		        

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
                                ; ???? NEEDED?
    jr z,interpret8
    cp '\r'                     ; carriage return? ascii 13
    jr z,interpret7		        ; if anything else its macro/control 

    cp CTRL_E
    jp z,edit_
    cp CTRL_H
    jp z,backSpace_
    cp CTRL_J
    jp z,reEdit_
    ; cp CTRL_S
    ; jp z,printStack_

    ; DB     lsb(edit_)       ; ENQ ^E  5
    ; DB     lsb(reedit_)     ; LF  ^J 10
    ; DB     lsb(list_)       ; FF  ^L 12
    ; DB     lsb(printStack_) ; DLE ^P 16
    ; DB lsb(depth_)      ;\#3    ( -- val )    depth of data stack  
    ; DB lsb(printStack_)   ;\#4    ( -- )        non-destructively prints stack
    ; DB lsb(prompt_)     ;\#5    ( -- )        print MINT prompt 
    ; DB lsb(editDef_)    ;\#6    ( char -- )   edit command    
    ; DB lsb(aDup_)       ;\#7    ( adr -- )    dupe (used in asm tests)
    ; DB     lsb(newln_)      ;a4    \$  prints a newline to output	
    
    ; reedit_: DB "\\e\\@\\#6;"			; lastDef, edit. remembers last line edited
    ; edit_: .cstr "`?`?\\#5\\#6;"      ; ?,key,prompt,edit 
    ; list_: .cstr "\\$26(\\i@65+\\#6\\c@0>(\\$))\\#5;" newln, loop 26 (index + 'A', edit tib* > 0, newln),prompt
    ; printStack_: .cstr "\\#4\\#5;"  ; print stack, prompt


interpret5a:    
    ; ld (vTIBPtr),bc
    ; ld bc,(vTIBPtr)
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
next:        
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

; edit 
edit_:                        
    jp interpret

reEdit_:
    jp interpret

; editDef:
;     pop hl                      ; pop ret address
;     ex (sp),hl                  ; swap with TOS                  
;     push hl                     ; dup TOS
;     ld a,l                      ; a = ident
;     ld de,TIB                   ; de = start of TIB
;     ld (vTIBPtr),de             ; update TIB*
;     push ix                     ; save NEXT
;     ld ix,editDef0              ; NEXT = editDef0
;     jp lookupRef                ; convert letter into address
; editDef0:
;     ld e,(hl)                   ; de = (hl++)
;     inc hl
;     ld d,(hl)
;     ld a,d                      ; de == 0 ?
;     or e
;     jr z,editDef4
;     ld ix,editDef3              ; NEXT = editDef3
;     ex de,hl
;     ld a,(hl)
;     cp "{"
;     jr nz,editDef1
;     jp editBlock0               ; convert letter into address
; editDef1:
;     cp "("
;     jr nz,editDef2
;     jp editBlock0               ; convert letter into address
; editDef2:
;     jp editFunc
; editDef3:
;     ld a," "                    ; write assign
;     call writeChar
;     pop hl                      ; a = ident
;     ld a,l
;     call writeChar
;     ld a,"="                    ; write assign
;     call writeChar
;     ld ix,editDef4              ; NEXT = editDef4
;     jp printTIB
; editDef4:
;     pop ix                      ; restore NEXT
;     jp (ix)

; writeChar:
;     ld de,(vTIBPtr)             ; de = TIB*
;     ld (de),a                   ; write ident
;     inc de
;     ld (vTIBPtr),de             ; update TIB* to point to end of string
;     ret

; ; printTIB
; printTIB:
;     ld hl,(vTIBPtr)
;     ld de,TIB
;     or a
;     sbc hl,de
;     jp printTIB2
; printTIB1:
;     ld a,(de)
;     call putchar
; printTIB2:
;     ld a,l
;     or h
;     jr nz,printTIB1
;     jp (ix)

; editBlock:
;     pop hl                      ; hl = block*
; editBlock0:
;     push ix                     ; save next
;     push hl                     ; push block*
;     push hl                     ; push block*
;     ld ix,(editBlock2)
;     jp blockLength
; editBlock1:
;     pop hl                      ; bc = length, (sp) = IP
;     pop de                      ; de = block*
;     ld a,l
;     or h
;     jr z,editBlock2
;     push bc                      
;     ld bc,hl                     
;     ex de,hl                    ; hl = block*
;     ld de,(vTIBPtr)              ; de = TIB*
;     ldir                        ; copy block to TIB
;     ld (vTIBPtr),de              ; save TIB*
;     pop bc
; editBlock2:
;     pop ix                      ; restore next
;     jp (ix)

; editFunc:
    
;     jp (ix)

; editArray:
;     jp (ix)

; editparseArgs:
;     jp (ix)

; ; blockLength
; ; addr1 -- length
; blockLength:
;     pop hl                      ; block*
;     push hl                     ; save block*
;     inc hl                      ; skip first char
;     ld d,1                      ; increase nesting
; blockLength1:                   ; Skip to end of definition    
;     ld a,(hl)                   ; Get the next character
;     inc hl                      ; Point to next character
;     cp " " + 1                  ; ignore whitespace 
;     jr c,blockLength1

;     cp ")"
;     jr z,blockLength4
;     cp "}"                       
;     jr z,blockLength4
;     cp "]"
;     jr z,blockLength4

;     cp "("
;     jr z,blockLength2
;     cp "{"
;     jr z,blockLength2
;     cp "["
;     jr z,blockLength2

;     cp "'"
;     jr z,blockLength3
;     cp "`"
;     jr z,blockLength3
;     cp DQ
;     jr z,blockLength3
;     jr blockLength1
; blockLength2:
;     inc d
;     jr blockLength1                   
; blockLength4:
;     dec d
;     jr nz, blockLength1         ; get the next element
; blockLength3:
;     ld a,$80
;     xor d
;     ld d,a
;     jr nz, blockLength1
;     pop hl                      ; hl = block*
;     or a                        
;     sbc hl,de
;     push hl
;     jp (ix)

    ; "`=> `\\a@2-\\#3 1-(",$22,"@\\b@(,)(.)2-)'\\$"             
    ; \a start of stack \#3 depth \b base \$ prompt
    
    ; DW dStack               ; a vS0 start of datastack			
    ; DW FALSE                ; b vBase16 
    ; DW 0                    ; c vTIBPtr an offset to the tib
    ; DW 0                    ; d 
    ; DW 65                   ; e vLastDef "A" last command u defined
    ; DW 0                    ; f 
    ; DW page6                ; g 256 bytes limits
    ; DW HEAP                 ; h vHeapPtr \h start of the free mem
