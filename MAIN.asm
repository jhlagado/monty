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
;  Inspiration from Charles H. Moore and Peter Jakacki
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

; z80_RST8    equ     $CF

; **************************************************************************
; Page 0  Initialisation
; **************************************************************************		

.org ROMSTART + $180		    ; 0+180 put monty code from here	

; **************************************************************************
; this code must not span pages
; **************************************************************************
macros:

; ***********************************************************************
; Initial values for system vars		
; ***********************************************************************		
isysVars:			            
    DW 2                        ; vDataWidth in bytes of array operations (default 1 byte) 
    DW 10                       ; vNumBase = 10
    DW TIB                      ; vTIBPtr pointer into TIB
    DW BUF                      ; vBUFPtr pointer into BUF
    DW next                     ; nNext
    DW HEAP                     ; vHeapPtr \h start of the free mem

    .align $100

opcodes:                        ; still available ~ ` _ 
    DB lsb(nop_)                ; SP  
    DB lsb(bang_)               ; !  
    DB lsb(dquote_)           ; "
    DB lsb(hash_)               ; #
    DB lsb(dollar_)             ; $  
    DB lsb(percent_)            ; %  
    DB lsb(amper_)              ; &
    DB lsb(tick_)               ; '
    DB lsb(lparen_)             ; (    
    DB lsb(rparen_)             ; )
    DB lsb(star_)               ; *  
    DB lsb(plus_)               ; +
    DB lsb(comma_)              ; , 
    DB lsb(minus_)              ; -
    DB lsb(dot_)                ; .
    DB lsb(slash_)              ; /	
    DB lsb(num_)                ; 0     
    DB lsb(num_)                ; 1    
    DB lsb(num_)                ; 2    
    DB lsb(num_)                ; 3    
    DB lsb(num_)                ; 4    
    DB lsb(num_)                ; 5    
    DB lsb(num_)                ; 6    
    DB lsb(num_)                ; 7    
    DB lsb(num_)                ; 8    
    DB lsb(num_)                ; 9    
    DB lsb(colon_)              ; :    
    DB lsb(semicolon_)          ; ;
    DB lsb(lt_)                 ; <
    DB lsb(eq_)                 ; =  
    DB lsb(gt_)                 ; >  
    DB lsb(question_)           ; ?    
    DB lsb(at_)                 ; @  
    DB lsb(upcase_)             ; A     
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
    DB lsb(lbrack_)             ; [
    DB lsb(backslash_)          ; \
    DB lsb(rbrack_)             ; ]
    DB lsb(caret_)              ; ^
    DB lsb(nop_)                ; _
    DB lsb(dquote_)             ; `     used for testing string   	    
    DB lsb(lowcase_)            ; a     
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
    DB lsb(lbrace_)             ; {
    DB lsb(pipe_)               ; |  
    DB lsb(rbrace_)             ; }  
    DB lsb(nop_)                ; ~    
    DB lsb(nop_)                ; DEL	


; **********************************************************************			 
; opcode landing page 
; **********************************************************************
    .align $100
page4:

plus_:                           ; add the top 2 members of the stack
    jp plus
at_:
    jp at
amper_:
    jp amper
dollar_:
    jp dollar
lbrack_:
    jp lbrack
rbrack_:
    jp rbrack
percent_:        
    jp percent 
backslash_:
    jp backslash
lparen_:
lbrace_:
    jp lbrace
rbrace_:
    jp rbrace
tick_:
    jp tick
semicolon_:
    jp semicolon
dot_:  
    jp dot
colon_:
    jp colon
upcase_:
    jp upcase
lowcase_:
    jp lowcase
question_:
    jp question
star_:    
    jp star 
bang_:				            ; logical invert, any non zero value 
    ld hl,0                     ; is considered true
    jr eq1    
num_:    
    jp  num
hash_:    
    jp hash
pipe_: 		 
    jp pipe
caret_: 		 
    jp caret
comma_: 		 
    jp comma
dquote_:
    jp dquote
minus_:
    jp minus
eq_:    
    inc bc
    ld a,(bc)                   ; is it == ?
    cp "="
    jr z,eq0                    ; no its equality
    dec bc
    jp assign                   ; no its assignment
eq0:
    pop hl
eq1:
    pop de
    jp equals

gt_:
    inc bc
    ld a,(bc)
    cp ">"
    jp z,shiftRight
    pop de
    pop hl
    jr lt1
lt_:
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
slash_:
    jp slash
nop_:  
    jp (ix)
rparen_:
    jp rparen

;*******************************************************************
; implementations
;*******************************************************************
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
    push hl        
    jp (ix)    

; @ addr
; -- ptr
at:
addr:
    ld de,(vPointer)
    ld hl,vPointer
    jp variable

amper:
and:
    pop de                      ; Bitwise and the top 2 elements of the stack
    pop hl     
    ld a,e        
    and l           
    ld l,a        
    ld a,d        
    and h           
and1:
    ld h,a        
    push hl        
    jp (ix)    
    
pipe:
or:
    pop de                      ; Bitwise or the top 2 elements of the stack
    pop hl
    ld a,e
    or l
    ld l,a
    ld a,d
    or h
    jr and1

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
    push hl        
    jp (ix)    
invert:				            ; Bitwise INVert the top member of the stack
    ld de, $FFFF                ; by xoring with $FFFF
    jr xor1    

; %a .. %z
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

rbrack:
arrayEnd:
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


; index of an array, based on vDataWidth 
; array num -- value    ; also sets vPointer to address 
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

; value _oldValue --            ; uses address in vPointer
assign:
    pop hl                      ; discard last accessed value
    pop hl                      ; hl = new value
assign0:
    ex de,hl                    ; de = new value
assignx:
    ld hl,(vPointer)     
    ld (hl),e
    ld a,(vDataWidth)                   
    dec a                       ; is it byte?
    jr z,assign1
    inc hl    
    ld (hl),d
assign1:	  
    jp (ix)  

; arg_list - parses input (ab:c)
; names after the : represent uninitialised locals
; return values are the state of the stack after the block ends
; format: numLocals totNumArgs argChars...
colon:
arglist:
    ld de,0                     ; d = count locals, e = count args ()
    ld hl,(vHeapPtr)            ; hl = heap*
    push hl                     ; save start of arg_list
    inc hl                      ; skip length fields to start of string
    inc hl
    inc bc                      ; point to next char
arglist1:
    ld a,(bc)
    cp ":"                      ; ":" switches from args to locals
    jr nz,arglist1a
    inc d                       ; non zero value local count acts as flag
    jr arglist3
arglist1a:
    cp "a"                      ; < "a" terminates arg_list
    jr c,arglist4
    cp "z"+1                    ; > "z" terminates arg_list
    jr nc,arglist4
arglist2:
    ld (hl),a
    inc hl                      
    inc e                       ; increase arg count
    xor a
    or d
    jr z,arglist3
    inc d                       ; if d > 0 increase local count
arglist3:
    inc bc                      ; point to next char
    jr arglist1
arglist4:
    dec bc
    xor a
    or d
    jr z,arglist5
    dec d                       ; remove initial inc
arglist5:
    inc hl
    ld (vHeapPtr),hl            ; bump heap* to after end of string
    pop hl                      ; hl = start of arg_list
    push hl                     ; return start of arg_list    
    ld (hl),d                   ; write number of locals at start - 1                                      
    inc hl                      
    ld (hl),e                   ; write number of args + locals at start - 2
    jp (ix)  

lbrace:
blockStart:
    push bc                     ; return pointer to first { of block    
    inc bc
    ld d,1                      ; nesting: count first parenthesis
blockStart1:                         ; Skip to end of definition    
    ld a,(bc)                   ; Get the next character
    inc bc                      ; Point to next character
    cp " " + 1                  ; ignore whitespace 
    jr c,blockStart1

    cp ")"
    jr z,blockStart4
    cp "}"                       
    jr z,blockStart4
    cp "]"
    jr z,blockStart4

    cp "("
    jr z,blockStart2
    cp "{"
    jr z,blockStart2
    cp "["
    jr z,blockStart2

    cp "'"
    jr z,blockStart3
    cp "`"
    jr z,blockStart3
    cp DQ
    jr z,blockStart3
    jr blockStart1
blockStart2:
    inc d
    jr blockStart1                   
blockStart3:
    ld a,$80
    xor d
    ld d,a
    jr nz, blockStart1
    jr blockStart5
blockStart4:
    dec d
    jr nz, blockStart1          ; get the next element
blockStart5:
    ld hl,bc                    ; hl = IP
    ld de,HEAP                  ; is IP pointing to object in heap
    or a                        ; IP - HEAP
    sbc hl,de
    bit 7,h                     ; if -ve then copy to heap else skip
    jr z,blockStart6
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
blockStart6:
    dec bc                      ; balanced, exit
    jp (ix)  

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
    jp (ix)    

tick:
char:
    ld hl,0                     ; if '' is empty or null
char1:
    inc bc                      ; point to next char
    ld a,(bc)
    cp "'"                      ; ' is the terminator
    jr z,char3
    cp $5c                      ; \ is the escape
    jr nz,char2
    inc bc
    ld a,(bc)
char2:
    ld l,a
    jr char1
char3:
    push hl
    jp (ix)  

backslash:
    jp (ix)

; , discard stack item
; x y -- x
comma:
discard:
    ld d,iyh                    ; limit this to SP <= BP
    ld e,iyl
    ex de,hl
    or a
    sbc hl,sp
    bit 7,h
    jr nz,discard1
    pop hl
discard1:
    jp (ix)

slash:
command:
    call jumpTable
    db "/"                      ; // comment
    dw comment
    db "a"                      
    dw command_a
    db "b"
    dw command_b
    db "c"                      ; /c chars
    dw chars
    db "d"                      ; /d decimal
    dw decimal
    db "f"                      ; /f false
    dw false1
    db "h"                      ; /h hexadecimal
    dw hexadecimal
    db "i"
    dw command_i
    db "k"                      ; /k key
    dw key
    db "n"                      ; /n numbers
    dw numbers
    db "o"                      ; /o output
    dw output
    db "p"                      
    dw command_p
    db "r"
    dw command_r
    db "s"                      ; /s size
    dw size
    db "t"                      ; /t true
    dw true1
    db "v"
    dw command_v
    db "x"                      ; /x xor
    dw xor
    db NUL
    dw div

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

dot:
    call jumpTable
    db "a"                      ; .a print array
    dw bufferArray
    db "c"                      ; .c print char
    dw bufferChar
    db "s"                      ; .s print string
    dw bufferString
    db "x"                      ; .x print x chars
    dw bufferXChars
    db NUL                      ; .  print number
    dw bufferNumber

; division subroutine.
; bc: divisor, de: dividend, hl: remainder

divide:        
    ld hl,0    	                        ; zero the remainder
    ld a,16    	                        ; loop counter
divide1:		                        ; shift the bits from bc (numerator) into hl (accumulator)
    sla c
    rl b
    adc hl,hl
    sbc hl,de		                    ; check if remainder >= denominator (hl>=de)
    jr c,divide2
    inc c
    jr divide3
divide2:		                        ; remainder is not >= denominator, so we have to add de back to hl
    add hl,de
divide3:
    dec a
    jr nz,divide1
    ld de,bc                              ; result from bc to de
    ret

; hl = value1, de = value2
; hl = result
equals:
    or a                        ; reset the carry flag
    sbc hl,de                   ; only equality sets hl=0 here
    jr z, true1
    jp false1

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

; execute a block of code which ends with }
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
    jr z,goBlock1
    cp "("
    jp nz,goFunc
    ; inc de                      ; de is the address to jump back to
    push de                     ; push de just before stack frame
goBlock:
goBlock1:    
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
    ld bc,de                    ; bc = de = block*-1
    jp (ix)    
    
goFunc:				        ; execute func
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
    jr goFunc2                ; push each item on stack
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
goFunc4:                      ; allocate locals 
    ld a,(hl)                   ; a = num_locals*, de = hblock* hl = arg_list*
    jr goFunc6
goFunc5:                      ; loop
    dec sp
    dec sp
    dec a
goFunc6:
    or a
    jr nz,goFunc5             ; end loop
goFunc7:
    inc hl                      ; a = num_args* x 2 
    ld a,(hl)
    dec hl
    add a,a                     ; a *= 2
goFunc8:
    push bc                     ; push IP
    ld bc,hl
    ld hl,2                     ; hl = first_arg* (BP+8), a = num args offset
    add a,l                     
    ld l,a
    add hl,sp
    jr goBlock2

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
upcase:
    ld a,(bc)                   ; a = identifier char
    sub 'A'                     ; 'A' = 0
    jr ident1
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

; if
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
; ifte
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

star:
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

rparen:
    ld c,(iy+8)                 ; IP = block* just under stack frame
    ld b,(iy+9)
    jp (ix)

; ";" createFunc
; arg_list* block* -- func*
semicolon:
createFunc:
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
    jp (ix)

; shiftLeft  
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

; shiftRight  
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

; string
; -- ptr                        ; points to start of string chars, 
                                ; length is stored at start - 2 bytes 
dquote:
string:     
    ld hl,(vHeapPtr)            ; hl = heap*
    inc hl                      ; skip length field to start
    inc hl
    push hl                     ; save start of string 
    inc bc                      ; point to next char
    jr string2
string1:
    ld (hl),a
    inc hl                      ; increase count
    inc bc                      ; point to next char
string2:
    ld a,(bc)
    cp DQ                      ; " is the string terminator
    jr z,string3
    cp "`"                      ; ` is the string terminator used in testing
    jr nz,string1
string3:
    xor a                       ; write NUL to terminate string
    ld (hl),a                   ; hl = end of string
    inc hl
    ld (vHeapPtr),hl            ; bump heap* to after end of string
    dec hl                      ; hl = end of string without terminator
    pop de                      ; de = start of string
    push de                     ; return start of string    
    or a                        ; hl = length bytes, de = start of string
    sbc hl,de
    ex de,hl
    dec hl                      ; write length bytes to length field at start - 2                                      
    ld (hl),d
    dec hl
    ld (hl),e
    jp (ix)  

minus:  		                ; negative sign or subtract
    inc bc                      ; check if sign of a number
    ld a,(bc)
    dec bc
    cp "0"
    jr c,sub
    cp "9"+1
    jp c,num_    
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
    jp add3

error1:
    ld hl,1                     ; error 1: unknown command
    push hl
    jp error

comment:
    inc bc                      ; point to next char
    ld a,(bc)
    cp " "                      ; terminate on any char less than SP 
    jr nc,comment
    dec bc
    jp (ix) 

command_a:
    call jumpTable
    db "b"                      ; /ab absolute
    dw absolute
    db "d"                      ; /ad address of
    dw addrOf
    db NUL
    dw error1

; /ab absolute
; num -- num
absolute:
    pop hl
    bit 7,h
    ret z
    xor a  
    sub l  
    ld l,a
    sbc a,a  
    sub h  
    ld h,a
    push hl
    jp (ix)

; /ad addrOf
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

command_b:
    call jumpTable
    db "r"                      ; /br break
    dw break
    db "y"                      ; /by cold boot
    dw coldStart
    db NUL
    dw error1

FUNC bufferArray, 2, "abc"
.cstr "{",DQ,"[ ",DQ,".s %a /s%c= 0%b= (%a %b #. %b ++ %b %c </br)^ ",DQ,"]",DQ,".s}",0
; .cstr "{$a/s$c= 0$b=( $a$b%/bd $b++ $b $c</br )^}" ; block

; /bd buffer decimal
; value --                      
bufferNumber:        
    ld a,(vNumBase)
    cp 16
    jr z,bufferHex              ; else falls through

; /bd buffer decimal
; value --                      
bufferDec:        
    ld de,(vBufPtr)             ; de'= buffer* bc' = IP
    exx                          
    pop hl                      ; hl = value
    call bufferDec0
    exx                         ; de = buffer*' bc = IP
    ld a," "                    ; append space to buffer
    ld (de),a
    inc e                       ; buffer*++, wraparound
    call z,flushBuffer
    ld hl,(vBufPtr)             ; hl = buffer*
    ld (vBufPtr),de             ; update buffer* with buffer*'
    jp (ix)

; hl = value
; de' = buffer*
; a, bc, de, hl destroyed
bufferDec0:    
    bit 7,h
    jr z,bufferDec1
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
bufferDec1:        
    ld c,0                      ; leading zeros flag = false
    ld de,-10000
    call bufferDec2
    ld de,-1000
    call bufferDec2
    ld de,-100
    call bufferDec2
    ld e,-10
    call bufferDec2
    inc c                       ; flag = true for at least digit
    ld e,-1
    call bufferDec2
    ret
bufferDec2:	     
    ld b,'0'-1
bufferDec3:	    
    inc b
    add hl,de
    jr c,bufferDec3
    sbc hl,de
    ld a,'0'
    cp b
    jr nz,bufferDec4
    xor a
    or c
    ret z
    jr bufferDec5
bufferDec4:	    
    inc c
bufferDec5:	    
    ld a,b
    exx
    ld (de),a
    inc e
    call z,flushBuffer
    exx
    ret

; /bh buffer hex
; value --                      
bufferHex:                      
    pop hl                      ; hl = value
    ld de,(vBufPtr)
    ld a,"$"                    ; # prefix
    ld (de),a
    inc e                       ; buffer*++, wraparound
    call z,flushBuffer
    ld a,h
    call bufferHex1
    ld a,l
    call bufferHex1
    ld a," "                    ; append space to buffer
    ld (de),a
    inc e                       ; buffer*++, wraparound
    call z,flushBuffer
    ld (vBufPtr),de
    jp (ix)

bufferHex1:		     
    push af
	rra 
	rra 
	rra 
	rra 
    call bufferHex2
    pop af
bufferHex2:		
    and	0x0F
	add	a,0x90
	daa
	adc	a,0x40
	daa
	ld (de),a
    inc e                       ; buffer*++, wraparound
    call z,flushBuffer
	ret

; /br break from loop             
; --
break:
    pop hl
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

; /bs buffered string             
; string* --
bufferString:
    pop hl                      ; hl = string*
    ld de,(vBufPtr)             ; de = buffer*
    jr bufferString1
bufferString0:
    ld (de),a                   ; a -> buffer*
    inc e                       ; buffer*++, wraparound
    call z,flushBuffer
    inc hl
bufferString1:
    ld a,(hl)                   ; a <- string*
    or a                        ; if NUL exit loop
    jr nz,bufferString0
    ld hl,(vBufPtr)             ; de = buffer*' hl = buffer*
    ld (vBufPtr),de             ; save buffer*' in pointer
    jp (ix)

; /bc buffer char             
; char -- 
bufferChar:
    ld hl,1
    jr bufferXChars0

; /bx buffered x chars             
; char length --
bufferXChars:
    pop hl                      ; hl = length
bufferXChars0:
    pop de                      ; a' = char
    ld a,e
    ex af,af'
    ld de,(vBufPtr)             ; de = buffer*
    jr bufferXChars2
bufferXChars1:
    ex af,af'
    ld (de),a
    ex af,af'
    inc e                       ; buffer*++, wraparound
    call z,flushBuffer
    dec hl
bufferXChars2:
    ld a,l
    or h
    jr nz,bufferXChars1
    ld (vBufPtr),de             ; save buffer*'
    jp (ix)

command_i:
    call jumpTable
    db "n"                      ; /in input
    dw input
    db "v"                      ; /iv invert
    dw invert
    db NUL
    dw error1

command_p:
    call jumpTable
    db "b"                      ; /pb print buffer
    dw printBuffer
    db "c"                      ; /pc print chars
    dw printChars
    db "k"                      ; /pk print stack
    dw printStack
    db NUL
    dw error1

; /pb printBuffer
; --
; prints chars in buffer from /vB to /vb. Resets /vb to /vB

FUNC printBuffer, 0, "a"
.cstr "{/vB /vb/vB- /pc /vB/vb=}"   ; block

; printChars
; char* len --
printChars:
    pop hl                              ; hl = count
    pop de                              ; de = char*
    call printChars2
    jp (ix)

printChars1:
    ld a,(de)                           ; print char at char*
    call putchar
    inc de                              ; char*++
    dec hl                              ; count--
printChars2:
    ld a,l                              ; count == 0?
    or h
    ret z
    jr printChars1                      ; if not loop

; /pk print stack
; -- 
printStack:
;     ld (vTemp1),bc
;     call printStr
;     .cstr "=> "
;     ld hl,STACK
;     sbc hl,sp
;     srl h
;     rr l
;     ld bc,hl
;     ld hl,STACK
;     jr printStack2
; printStack1:
;     dec bc
;     dec hl
;     ld d,(hl)
;     dec hl
;     ld e,(hl)
;     ex de,hl    
;     call prthex
;     ex de,hl
;     ld a," "
;     call putchar
; printStack2:
;     ld a,c
;     or b
;     jr nz,printStack1
;     call prompt
;     ld bc,(vTemp1)
    jp (ix)

command_r:
    call jumpTable
    db "e"                      ; /re remainder
    dw remain
    db NUL
    dw error1

remain:
    ld hl,(vRemain)
    push hl
    jp (ix)

size:
    pop hl
    dec hl                      ; msb size 
    ld d,(hl)
    dec hl                      ; lsb size 
    ld e,(hl)
    push de
    jp (ix)

command_v:
    call jumpTable
    db "b"
    dw varBufPtr
    db "h"
    dw varHeapPtr
    db "t"
    dw varTIBPtr
    db "B"
    dw constBufStart
    db "T"
    dw constTIBStart
    db NUL
    dw error1

constBufStart:
    ld de,BUF
    jr constant

constHeapStart:
    ld de,HEAP
    jr constant

constTIBStart:
    ld de,TIB
    jr constant

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


chars:
    ld hl,1
chars1:
    ld (vDataWidth),hl
    jp (ix)

decimal:
    ld hl,10
decimal1:
    ld (vNumBase),hl
    jp (ix)
hexadecimal:
    ld hl,16
    jp decimal1

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

key:
    call getchar
    ld h,0
    ld l,a
    push hl
    jp (ix)

; Z80 port output
; value port --
output:
    pop hl
    ld e,c                      ; save IP
    ld c,l
    pop hl
    out (c),l
    ld c,e                      ; restore IP
    jp (ix)    

numbers:
    ld hl,2
    jp chars1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; /fs funcSrc
; func -- src
FUNC funcSrc, 1, "f"                    ; :f func or block                 
.cstr 
"{"
    ":kt{",                             ; :kt sink, type 
        "0%t==/br",                     ; break if t != 0 
        ":dt{",
            "1%t==/br %f^ 1 %k^",       ; if t == 1 send data to sink
        "} 0 %k^",                      ; init sink
    "}", 
"}" 

; /mp map
; func -- :s{:mk}
FUNC map, 0, "f"                        ; :f func 
.cstr 
"{"
    ":s{",                              ; :s source 
        ":kt{",                         ; :kt sink, type 
            "0%t==/br",                 ; break if t != 0 
            ":dt{",
                "1%t=={%d %f^}{%d}??",  ; if t == 1 pass data through func else raw
                "%t %k^",               ; send data to sink
            "} 0 %s^",                  ; init source
        "}", 
    "}", 
"}" 

; /fe forEach
; :o -- :s
FUNC forEach, 0, "p"                    ; :p proc 
.cstr 
"{"
    ":s:T{",                            ; :s source 
        "[0]%T=",
        "0%t==/br",                     ; break if t != 0 
        ":dt{",
            "{ 0%t==/br %d %T0#= }",    ; 0: store talkback
            "{ 1%t==/br %d %p^ }",      ; 1: send data to proc
            "{ 2%t!=/br 0 1 %T0#^ }",   ; 0 or 1: get next data item
        "} 0 %s^",                      ; init source
    "}", 
"}" 

; example {/k}/fs :a{a}/mp^ {.}/fe^
; example [ {/k}/fs :a{a}/mp {.}/fe ] /pi

filter:
scan:
    jp (ix)

;*******************************************************************
; general routines
;*******************************************************************

; prints whatever in in buffer starting from BUF and ending at vBufPtr* 
flushBuffer:
    push af
    push de
    push hl
    ld hl,(vBufPtr)
    ld de,BUF
    ld (vBufPtr),de
    or a
    sbc hl,de
    call printChars2
    pop hl
    pop de
    pop af
    ret

; followed by a table
; db char
; dw addr
; the final item must have char == NUL
jumpTable:
    inc bc
    pop hl
jumpTable0:
    xor a
    cp (hl)
    jr z,jumpTable1
    ld a,(bc)
    cp (hl)
    jr z,jumpTable2
    inc hl
    inc hl
    inc hl
    jr jumpTable0
jumpTable1:
    dec bc
jumpTable2:
    inc hl
    ld e,(hl)
    inc hl
    ld d,(hl)
    ex de,hl
    jp (hl)

prtstr0:
    call putchar
    inc hl
prtstr:
    ld a,(hl)
    or a
    jr nz,prtstr0
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
    cp DQ                      ; quote char
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
    call prtstr		
    inc hl			            ; inc past NUL
    ex (sp),hl		            ; put it back	
    ret

titleStr:
    .cstr ESC,"[2JMonty V0.1\r\n",0,0,0

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
    ld bc,8 * 2
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
    call prtstr 		        ; prog count to stack, put code line 235 on stack then call print

interpret:

    call flushBuffer
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
    cp CTRL_S
    jp z,printStack_

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
    sub " "
    ld l,a                      ; index into table
    ld h,msb(opcodes)           ; start address of jump table    
    ld l,(hl)                   ; get low jump address
    ld h,msb(page4)             ; Load h with the 1st page address
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
    call run
    db DQ,"Error ",DQ,".s .",0
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
    call run
    db DQ,"var?",DQ,".s /k/ad .h",0
    jp interpret

reEdit_:
    jp interpret

printStack_:
    call run
    .cstr "/pk"
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

; editArglist:
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
