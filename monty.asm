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

DSIZE       EQU     $80
TIBSIZE     EQU     $100	    ; 256 bytes , along line!
TRUE        EQU     -1		    ; C-style true
FALSE       EQU     0
EMPTY       EQU     0		         
UNUSED      EQU     $ff
NUL         EQU     0           ; exit code
DC1         EQU     17          ; ?
DC2         EQU     18          ; ?
DC3         EQU     19          ; ?
ESC         EQU     27          ; ?
DQUOTE      EQU     $22         ; " double quote char

z80_RST8    EQU     $CF
; **************************************************************************
; stack frame
;
; arg0                              -- 0th arg
; arg1
;  :
; argn                              -- nth arg
; loc0                              -- 0th local
; loc1
;  :
; locn                              -- last local             
; IP                                -- IP (saved interpreter ptr, return)
; arg_list*                         -- arg_list*
; first_arg*                        -- first_arg*           
; BP                                -- BP (saved base ptr)           <-- iy
; res0                              -- 0th result
; res1
;  :
; resn                              -- last result.             <-- sp
;
; **************************************************************************

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
    DW 2                        ; b vDataWidth in bytes of array operations (default 1 byte) 
    DW 0                        ; c vTIBPtr an offset to the tib
    DW next                     ; g nNext
    DW heap                     ; h vHeapPtr \h start of the free mem

    .align $100

opcodesBase:

ctrlCodes:
    DB lsb(EMPTY)               ; ^@  0 NUL  
    DB lsb(EMPTY)               ; ^A  1 SOH
    DB lsb(EMPTY)               ; ^B  2 STX
    DB lsb(EMPTY)               ; ^C  3 ETX
    DB lsb(EMPTY)               ; ^D  4 EOT
    DB lsb(EMPTY)               ; ^E  5 ENQ
    DB lsb(EMPTY)               ; ^F  6 ACK
    DB lsb(EMPTY)               ; ^G  7 BEL
    DB lsb(EMPTY)               ; ^H  8 BS
    DB lsb(EMPTY)               ; ^I  9 TAB
    DB lsb(EMPTY)               ; ^J 10 LF
    DB lsb(EMPTY)               ; ^K 11 VT
    DB lsb(EMPTY)               ; ^L 12 FF
    DB lsb(EMPTY)               ; ^M 13 CR
    DB lsb(EMPTY)               ; ^N 14 SO
    DB lsb(EMPTY)               ; ^O 15 SI
    DB lsb(EMPTY)               ; ^P 16 DLE
    DB lsb(EMPTY)               ; ^Q 17 DC1    
    DB lsb(EMPTY)               ; ^R 18 DC2   
    DB lsb(EMPTY)               ; ^S 19 DC3  
    DB lsb(EMPTY)               ; ^T 20 DC4  
    DB lsb(EMPTY)               ; ^U 21 NAK     
    DB lsb(EMPTY)               ; ^V 22 SYN
    DB lsb(EMPTY)               ; ^W 23 ETB  
    DB lsb(EMPTY)               ; ^X 24 CAN   
    DB lsb(EMPTY)               ; ^Y 25 EM  
    DB lsb(EMPTY)               ; ^Z 26 SUB  
    DB lsb(EMPTY)               ; ^[ 27 ESC
    DB lsb(EMPTY)               ; ^\ 28 FS
    DB lsb(EMPTY)               ; ^] 29 GS
    DB lsb(EMPTY)               ; ^^ 30 RS
    DB lsb(EMPTY)               ; ^_ 31 US

opcodes:                        ; still available , ;  
    DB lsb(nop_)                ; SP  
    DB lsb(not_)                ; !  
    DB lsb(string_)             ; "
    DB lsb(hexnum_)             ; #
    DB lsb(arg_)                ; $  
    DB lsb(arrIndex_)           ; %  
    DB lsb(and_)                ; &
    DB lsb(char_)               ; '
    DB lsb(arg_list_)           ; (    
    DB lsb(nop_)                ; )
    DB lsb(mul_)                ; *  
    DB lsb(add_)                ; +
    DB lsb(nop_)                ; , 
    DB lsb(sub_)                ; -
    DB lsb(dot_)                ; .
    DB lsb(div_)                ; /	
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
    DB lsb(go_)                 ; :    
    DB lsb(nop_)                ; ;
    DB lsb(lt_)                 ; <
    DB lsb(eq_)                 ; =  
    DB lsb(gt_)                 ; >  
    DB lsb(if_)                 ; ?    
    DB lsb(addr_)               ; @  
    DB lsb(identU_)             ; A     
    DB lsb(identU_)             ; B     
    DB lsb(identU_)             ; C     
    DB lsb(identU_)             ; D     
    DB lsb(identU_)             ; E     
    DB lsb(identU_)             ; F     
    DB lsb(identU_)             ; G     
    DB lsb(identU_)             ; h     
    DB lsb(identU_)             ; I     
    DB lsb(identU_)             ; J     
    DB lsb(identU_)             ; K     
    DB lsb(identU_)             ; L     
    DB lsb(identU_)             ; M     
    DB lsb(identU_)             ; N     
    DB lsb(identU_)             ; O     
    DB lsb(identU_)             ; p     
    DB lsb(identU_)             ; Q     
    DB lsb(identU_)             ; R     
    DB lsb(identU_)             ; S     
    DB lsb(identU_)             ; T     
    DB lsb(identU_)             ; U     
    DB lsb(identU_)             ; V     
    DB lsb(identU_)             ; W     
    DB lsb(identU_)             ; X     
    DB lsb(identU_)             ; Y     
    DB lsb(identU_)             ; Z    
    DB lsb(arrBegin_)           ; [
    DB lsb(command_)            ; \
    DB lsb(arrEnd_)             ; ]
    DB lsb(xor_)                ; ^
    DB lsb(remain_)             ; _
    DB lsb(string_)             ; `     used for testing string   	    
    DB lsb(identL_)             ; a     
    DB lsb(identL_)             ; b  
    DB lsb(identL_)             ; c  
    DB lsb(identL_)             ; d  
    DB lsb(identL_)             ; e  
    DB lsb(identL_)             ; f  
    DB lsb(identL_)             ; g  
    DB lsb(identL_)             ; h  
    DB lsb(identL_)             ; i  
    DB lsb(identL_)             ; j  
    DB lsb(identL_)             ; k  
    DB lsb(identL_)             ; l  
    DB lsb(identL_)             ; m  
    DB lsb(identL_)             ; n  
    DB lsb(identL_)             ; o  
    DB lsb(identL_)             ; p  
    DB lsb(identL_)             ; q  
    DB lsb(identL_)             ; r  
    DB lsb(identL_)             ; s  
    DB lsb(identL_)             ; t  
    DB lsb(identL_)             ; u  
    DB lsb(identL_)             ; v  
    DB lsb(identL_)             ; w  
    DB lsb(identL_)             ; x  
    DB lsb(identL_)             ; y  
    DB lsb(identL_)             ; z  
    DB lsb(block_)              ; {
    DB lsb(or_)                 ; |  
    DB lsb(blockEnd_)           ; }  
    DB lsb(inv_)                ; ~    
    DB lsb(nop_)                ; DEL	


; **********************************************************************			 
; symbolic operators 
; **********************************************************************
    .align $100
page4:

add_:                           ; add the top 2 members of the stack
    pop de        
    pop hl        
    add hl,de    
    push hl        
    jp (ix)    
addr_:
    jp addr
and_:
    jp and
arg_:
    jp arg
arg_list_:    
    jp arg_list
arrBegin_:
    jp arrBegin
arrEnd_:
    jp arrEnd
arrIndex_:        
    jp arrIndex 
block_:
    jp block
blockend_:
    jp blockend
char_:
    jp char
command_:
    jp command
dot_:  
    jp dot
remain_:
    jp remain
go_:
    jp go
identU_:
    jp identU
identL_:
    jp identL
if_:
    jp if
inv_:				            ; Bitwise INVert the top member of the stack
    ld de, $FFFF                ; by xoring with $FFFF
    jp xor1    
mul_:    
    jp mul 
not_:				            ; logical invert, any non zero value 
    ld hl,0                     ; is considered true
    jr eq1    
num_:    
    jp  num
hexnum_:    
    jp hexnum
or_: 		 
    jp or
xor_: 		 
    jp xor
string_:
    jp string
sub_:  		                    ; negative sign or subtract
    inc bc                      ; check if sign of a number
    ld a,(bc)
    dec bc
    cp "0"
    jr c,sub1
    cp "9"+1
    jp c,num_    
sub1:                           ; Subtract the value 2nd on stack from top of stack 
    pop de    
    pop hl                      ; Entry point for INVert
sub2:    
    or a                        ; Entry point for NEGate
    sbc hl,de       
    push hl        
    jp (ix)        

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
    
div_:    
    pop  de                     ; get first value
    pop  hl                     ; get 2nd value
    push bc                     ; preserve the IP    
    ld bc,hl                
    call divide
    ld (vRemain),hl
    pop bc
    push de                     ; push result
    jp (ix)

nop_:  
    jp (ix)

;*******************************************************************
; word operators
;*******************************************************************

; -- ptr
addr:
    ld hl,(vPointer)
    push hl
    ld hl,vPointer
    ld (vPointer),hl
    jp (ix)
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
    xor     l
    ld l,a
    ld a,d
    xor     h
    jr and1

; $a .. $z
; -- value
; returns value of arg
arg:
    ld e,(iy+4)                 ; hl = arg_list* 
    ld d,(iy+5)
    ex de,hl                    
    ld a,l                      ; arg_list* == null, skip
    or h
    jr z,arg0a
    dec hl                      ; a = num_args, hl = arg_list*
    dec hl
    ld a,(hl)                    
    inc hl
    inc hl
    or a
    jr z,arg0a                  ; num_args == 0, skip 
    ld e,a                      ; e = a = num_args
    inc bc                      ; a = next char = arg_name
    ld a,(bc)
    push bc                     ; save IP                         
    ld b,e                      ; b = e = num_args
    ld e,(iy+2)                 ; de = first_arg*, hl = argslist*   
    ld d,(iy+3)
arg0:
    dec de                      ; a = arg_name, de = next arg*
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


; arg_list - parses input (ab:c)
; names after the : represent uninitialised locals
; return values are the state of the stack after the block ends

arg_list:
    ld de,0                     ; d = count locals, e = count args ()
    ld hl,(vHeapPtr)            ; hl = heap*
    inc hl                      ; skip length field to start
    inc hl
    push hl                     ; save start of arg_list
    inc bc                      ; point to next char
arg_list1:
    ld a,(bc)
    cp ")"                      ; ) is the arg_list terminator
    jr z,arg_list4
    cp ":"
    jr nz,arg_list2
    inc d                       ; non zero value local count acts as flag
    jr nz,arg_list3
arg_list2:
    ld (hl),a
    inc hl                      
    inc e                       ; increase arg count
    xor a
    or d
    jr z,arg_list3
    inc d                       ; if d > 0 increase local count
arg_list3:
    inc bc                      ; point to next char
    jr arg_list1
arg_list4:
    xor a
    or d
    jr z,arg_list5
    dec d                       ; remove initial inc
arg_list5:
    inc hl
    ld (vHeapPtr),hl            ; bump heap* to after end of string
    pop hl                      ; hl = start of arg_list
    push hl                     ; return start of string    
    dec hl                      ; write length bytes to length field at start - 2                                      
    ld (hl),d
    dec hl
    ld (hl),e
    jp (ix)  

arrBegin:
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

arrEnd:
    ld d,iyh                    ; de = BP
    ld e,iyl
    ld (vTemp1),bc              ; save IP
    ld hl,de                    ; hl = de = BP
    or a 
    sbc hl,sp                   ; hl = array count (items on stack)
    srl h                       ; 
    rr l                        
    ld bc,hl                    ; bc = count
    ld hl,(vHeapPtr)            ; hl = array[-2]
    ld (hl),c                   ; write num items in length word
    inc hl
    ld (hl),b
    inc hl                      ; hl = array[0], bc = count
                                ; de = BP, hl = array[0], bc = count
arrEnd1:                        
    ld a,(iy-2)                 ; a = lsb of stack item
    ld (hl),a                   ; write lsb of array item
    inc hl                      ; move to msb of array item
    ld a,(vDataWidth)           ; vDataWidth=1? 
    dec a
    jr z,arrEnd2
    ld a,(iy-1)                 ; a = msb of stack item
    ld (hl),a                   ; write msb of array item
    inc hl                      ; move to next word in array
arrEnd2:
    dec iy                      ; move to next word on stack
    dec iy
    dec bc                      ; dec items count
    ld a,c                      ; if not zero loop
    or b
    jr nz,arrEnd1
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
    ld (vHeapPtr),hl            ; move heap* to end of array
    ld bc,(vTemp1)              ; restore IP
    inc de                      ; de = array[0]
    inc de
    push de                     ; return array[0]
    jp (ix)


; index of an array, based on vDataWidth 
; array num -- value    ; also sets vPointer to address 
arrIndex:
    pop hl                              ; hl = index  
    pop de                              ; de = array
    ld a,(vDataWidth)                   ; a = data width
    dec a
    jr z,arrIndex1
arrIndex0:
    add hl,hl                           ; if data width = 2 then double 
arrIndex1:
    add hl,de                           ; add addr
    ld (vPointer),hl                    ; store address in setter    
    ld d,0
    ld e,(hl)
    or a                                ; check data width again                                
    jr z,arrIndex2
    inc hl
    ld d,(hl)
arrIndex2:
    push de
    jp (ix)


; value _oldValue --            ; uses address in vPointer
assign:
    pop hl                      ; discard last accessed value
    pop de                      ; new value
    ld hl,(vPointer)     
    ld (hl),e
    ld a,(vDataWidth)                   
    dec a                       ; is it byte?
    jr z,assign1
    inc hl    
    ld (hl),d
assign1:	  
    jp (ix)  

block:
    push bc                     ; return pointer to first { of block    
    inc bc
    ld d,1                      ; nesting: count first parenthesis
block1:                         ; Skip to end of definition    
    ld a,(bc)                   ; Get the next character
    inc bc                      ; Point to next character
    cp " " + 1                  ; ignore whitespace 
    jr c,block1

    cp ")"
    jr z,block4
    cp "}"                       
    jr z,block4
    cp "]"
    jr z,block4

    cp "("
    jr z,block2
    cp "{"
    jr z,block2
    cp "["
    jr z,block2

    cp "'"
    jr z,block3
    cp "`"
    jr z,block3
    cp $22
    jr z,block3
    jr block1
block2:
    inc d
    jr block1                   
block3:
    ld a,$80
    xor d
    ld b,a
    jr nz, block1
    jr block5
block4:
    dec d
    jr nz, block1               ; get the next element
block5:
    ld hl,bc                    ; hl = IP
    ld de,HEAP                  ; is IP pointing to object in heap
    or a                        ; IP - HEAP
    sbc hl,de
    bit 7,h                     ; if -ve then copy to heap else skip
    jr z,block6
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
block6:
    dec bc                      ; balanced, exit
    jp (ix)  

blockend:
    exx                         ; de' = oldBP bc' = oldIP
    ld e,(iy+0)                  
    ld d,(iy+1)
    ld c,(iy+6)                  
    ld b,(iy+7)
    exx
    ld e,(iy+2)                 ; hl = first_arg*, is it in this scope?
    ld d,(iy+3)
    ex de,hl                                                              
    ld e,(iy+0)                 ; de = oldBP
    ld d,(iy+1)
    inc de                      ; for carry flag <=
    or a
    sbc hl,de
    jr c,blockend1              ; oldBP >= first_arg, same scope skip
    ld d,iyh                    ; de = BP = first_result*, no args in this scope
    ld e,iyl
    ld hl,8
    add hl,de                   ; de = BP = first_result* (BP), hl = first_arg* (BP+8)
    ex de,hl                    ; de = first_arg*, hl = first_result*
    jr blockend2
blockend1:                      ; same scope
    ld e,(iy+2)                 ; hl = first_arg*, in scope
    ld d,(iy+3)
    ex de,hl                                                              
    ld d,iyh                    ; de = first_arg*, hl = BP = first_result*
    ld e,iyl
    ex de,hl                                                              
blockend2:                      
    ld bc,hl                    ; bc = hl = BP
    or a                        ; hl = BP - SP = count 
    sbc hl,sp                   
    ld a,l
    or h
    jr z,blockend3                      
    push bc                     ; bc = count, hl = BP
    ld bc,hl
    pop hl                      
    dec hl                      ; hl = BP-1
    dec de                      ; de = args*-1
    lddr
    inc de                      
blockend3:                      
    ex de,hl                    ; hl = new tos
    ld sp,hl                    ; sp = new tos
    exx                         ; bc = IP, iy = oldBP
    push de                     
    push bc                     
    exx 
    pop bc
    pop iy
    jp (ix)    

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

; ; ;
; ; block* -- hblock*
; ; copies bytes from TOS to IP to the heap
; compile:
    ;   ld (vTemp1),bc              ; save IP
    ; pop de                      ; de = block*
    ; ld hl,bc                    ; hl = IP
    ; or a                        ; bc = size
    ; sbc hl,de
    ; ld bc,hl
    ; ex de,hl                    ; hl = block*
    ; ld de,(vHeapPtr)            ; de = heap*
    ; push de                     ; return hblock*
    ; ldir                        ; copy size bytes from block* to hblock*
    ; ld (vHeapPtr),de
    ; ld bc,(vTemp1)              ; restore IP
    ; jp (ix)

dot:  
    pop hl
    inc bc
    ld a,(bc)
    cp "h"
    jr nz,dot1
    call prthex
    jr dot4
dot1:
    cp "s"
    jr nz,dot2
    call prtstr
    jr dot4
dot2:
    cp "c"
    jr nz,dot3
    ld a,l
    call putchar
    jr dot4
dot3:
    dec bc
    call prtdec
dot4:
    ld a,' '       
    call putchar
    jp (ix)

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

goFunc:				        ; execute code at pointer
    ex de,hl                    ; hl = func*
    ld e,(hl)                   ; de = hblock*
    inc hl
    ld d,(hl)
    inc hl
    push de                     ; save hblock*
    ld e,(hl)                   ; de = arg_list*
    inc hl
    ld d,(hl)
    inc hl
    ex de,hl                    ; hl = arg_list*
    pop de                      ; restore hblock*
    ld a,l                      ; if arg_list* != null skip
    or h
    jr nz,goFunc1              
    push bc                     ; push IP
    jr goBlock2                  
goFunc1:
    dec hl                      ; a = num_locals*, de = hblock* hl = arg_list*
    ld a,(hl)
    inc hl
    or a
    jr z,goFunc3
goFunc2:
    dec sp
    dec sp
    dec a
    jr nz,goFunc2
goFunc3:
    push bc                     ; push IP    
    push hl                     ; push arg_list*
    dec hl                      ; hl = num_args*
    dec hl
    ld a,(hl)                   ; hl = num_args * 2
    add a,a
    add a,4                     ; offset for IP and arg_list
    ld l,a
    ld h,$0
    add hl,sp                   ; hl = first_arg*
    jr goBlock4

goBlock:
    inc de
    push bc                     ; push IP
    ld hl,stack                 ; de = BP, hl = stack, (sp) = code*
    ld b,iyh                    
    ld c,iyl
    or a                        ; hl = stack - BP = root_scope
    sbc hl,bc                   
    ld a,l                      ; if root_scope, skip
    or h                    
    jr z,goBlock2
    ld c,(iy+4)                 ; push arg_list* (parent)
    ld b,(iy+5)                 
    push bc                     
    ld c,(iy+2)                 ; hl = first_arg* (parent)
    ld b,(iy+3)                 
    ld hl,bc
    jr goBlock3
goBlock2:
    push hl                     ; push arg_list (null)
    ld hl,4                     ; hl = first_arg* (BP+8)
    add hl,sp
goBlock3:
    dec de
goBlock4:
    push hl                     ; push first_arg    
    push iy                     ; push BP
    ld iy,0                     ; BP = SP
    add iy,sp
    ld bc,de                    ; bc = de = block*-1
    jp (ix)    

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
identU:
    ld a,(bc)                   ; a = identifier char
    sub 'A'                     ; 'A' = 0
    jr ident1
identL:
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
if:
    inc bc
    ld a,(bc)
    cp "?"
    jr z,ifte
    dec bc
    ld de,0                      ; NUL pointer for else
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
    jp z,go1                ; if z de = else                   
    ex de,hl                    ; condition = false, de = then  
    jp go1

mul:        
    pop  de                     ; get first value
    pop  hl
    push bc                     ; Preserve the IP
    ld b,h                      ; bc = 2nd value
    ld c,l
    
    ld hl,0
    ld a,16
mul2:
    add hl,hl
    rl e
    rl d
    jr nc,$+6
    add hl,bc
    jr nc,$+3
    inc de
    dec a
    jr nz,mul2
	pop bc			            ; Restore the IP
	push hl                     ; Put the product on the stack - stack bug fixed 2/12/21
	jp (ix)

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

remain:
    ld hl,(vRemain)
    push hl
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
    cp DQUOTE                      ; " is the string terminator
    jr nz,string1
    cp "`"                      ; ` is the string terminator used in testing
    jr nz,string1
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

;*******************************************************************
; commands
;*******************************************************************
command:
    inc bc
    ld a,(bc)
    cp $5C                      ; \\ comment
    jp z,comment
    cp "a"                      ; \a absolute
    jp z,abs1
    cp "b"                      ; \b bytes
    jp z,bytes
    cp "f"                      ; \f func
    jp z,func
    cp "F"                      ; \F false
    jp z,false1
    cp "i"                      ; \i input
    jp z,input
    cp "k"                      ; \k key
    jp z,key
    cp "o"                      ; \o output
    jp z,output
    cp "r"                      ; \r repeat
    jp z,repeat
    cp "s"                      ; \s select
    jp z,select
    cp "T"                      ; \T true
    jp z,true1
    cp "w"                      ; \w words
    jp z,words
    cp "x"                      ; \x exit loop or block
    jp z,blockExit

    ld hl,1                     ; error 1: unknown command
    jp error

abs1:
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

comment:
    inc bc                      ; point to next char
    ld a,(bc)
    cp " "                      ; terminate on any char less than SP 
    jr nc,comment
    dec bc
    jp (ix) 

bytes:
    ld hl,1
bytes1:
    ld (vDataWidth),hl
    jp (ix)

; arg_list* block* -- ptr
func:
    pop de                              ; de = block* hl = heap*
    ld hl,(vHeapPtr)
    ld (hl),e                           ; compile block*
    inc hl
    ld (hl),d
    inc hl
    pop de                              ; de = block*
    ld (hl),e                           ; compile arg_list*
    inc hl
    ld (hl),d
    inc hl
    ld de,(vHeapPtr)                    ; return func*
    push de
    ld (vHeapPtr),hl                    ; heap* += 4
    jp (ix)

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

; repeat
; block* -- 
repeat:
    dec bc                      ; rewind IP to before \r
    dec bc
    pop hl
    push hl
    push hl
    jp go    

; select
; index array -- value
select: 
    pop de                      ; de = array
    pop hl                      ; hl = index  
    add hl,hl                   ; if data width = 2 then double 
    add hl,de                   ; add addr
    ld e,(hl)
    inc hl
    ld d,(hl)
    jp go1

words:
    ld hl,2
    jp bytes1

blockExit:
    pop hl
    ld a,l
    or h
    jr z,blockExit1
    jp (ix)
blockExit1:    
    ld l,(iy+6)                 ; hl = oldIP
    ld h,(iy+7)
    inc hl                      ; forward IP on stack to after \r
    inc hl
    ld (iy+6),l                  
    ld (iy+7),h
    ld e,(iy+2)                 ; hl = first_arg*, is it in this scope?
    ld d,(iy+3)
    inc de
    inc de
    ld (iy+2),e                 ; hl = first_arg*, is it in this scope?
    ld (iy+3),d
    jp blockEnd

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; c b --
; loops until c = 0
loop:                           
    
    jp (ix)     
    
;     pop de                      ; de = block                    c
;     pop hl                      ; hl = condition    
;     push de
;     push bc                     ; push IP
;     ld bc,de                    ; bc = block
;     ld e,(iy+2)                 ; get first_arg* from parent stack frame
;     ld d,(iy+3)                 ; make this the old BP for this stack frame
;     push de                     ; push first_arg*
;     push iy                     ; push BP  
;     ld iy,0                     ; iy = sp
;     add iy,sp
; loop1:    
;     ld a,l                      ; bc = block, hl = condition = zero?
;     or h                        
;     jr z,loop3
;     ld de,loop2-1               ; IP return address
;     push de
;     ld e,(iy+2)                 ; push parent first_arg*
;     ld d,(iy+3)                  
;     push de                     ; 
;     push iy                     ; push BP  
;     ld iy,0                     ; iy = sp
;     add iy,sp
;     push hl                     ; push condition
;     dec bc
;     jp (ix)                     

; loop2:
;     db ESC                      ; escape from interpreter
;     ld c,(iy+6)                 ; bc = block
;     ld b,(iy+7)                  
;     pop hl                      ; hl = condition
;     jr loop1
    
; loop3:
;     ld d,iyh                    ; de = BP
;     ld e,iyl
;     ex de,hl                    ; hl = BP, de = result
;     ld sp,hl                    ; sp = BP
;     pop hl                      ; hl = old BP
;     pop bc                      ; pop first_arg* (discard)
;     pop bc                      ; bc = IP
;     ld sp,hl                    ; sp = old BP
;     ld iy,0                     ; iy = sp
;     add iy,sp
;     ld ix,(vNext)                  ; needed?
;     jp (ix)

;; str -- num
; hash:
    ; pop hl
    ; push bc
    ; ld bc,hl
    ; call hashStr
    ; pop bc
    ; push hl
    ; jp (ix)

; sqrt1:
;     pop hl
;     push bc
;     call squareRoot
;     ld (vRemain),bc
;     pop bc
;     push de
;     jp (ix)

filter:
map:
scan:
    jp (ix)


; -------------------------------------------------------------------------------


; ; hash C-string 
; ; BC = str
; ; HL = hash
; hashStr:
;     ld (vHashStr),bc                    ; store source string
;     ld hl,0                             
; hashStr1:    
;     ld a,(bc)                           ; load next char
;     inc bc
;     cp 0                                ; NUL?
;     ret z                     
; hashStr2:
;     ld d,0
;     ld e,a 
;     add hl,de
;     ld de,hl                            ; hl *= 193 (11000001)
;     add hl,hl                           ; shift left
;     add hl,de                           ; add
;     add hl,hl                           ; shift left
;     add hl,hl                           ; shift left
;     add hl,hl                           ; shift left
;     add hl,hl                           ; shift left
;     add hl,hl                           ; shift left
;     add hl,hl                           ; shift left
;     add hl,de                           ; add
;     jr hashStr1

; ; squareroot
; ; Input: HL = value
; ; Result: DE = square root BC = remainder

; squareRoot:
;     ld bc,0800h   
;     ld e,c        
;     xor a         
; squareRoot1:        
;     add hl,hl     
;     rl c          
;     adc hl,hl     
;     rl c          
;     jr nc,$+4     
;     set 0,l       
;     ld a,e        
;     add a,a       
;     ld e,a        
;     add a,a       
;     bit 0,l       
;     jr nz,$+5     
;     sub c         
;     jr nc,squareRoot4     
;     ld a,c         
;     sub e              
;     inc e          
;     sub e           
;     ld c,a         
; squareRoot4:
;     djnz squareRoot1
;     bit 0,l       
;     jr z,squareRoot5         
;     inc b         
; squareRoot5:
;     ld d,0
;     ret           

; print decimal
; hl = value
prtdec:        
    bit 7,h
    jr z,prtdec0
    ld a,'-'
    call putchar
    xor a  
    sub l  
    ld l,a
    sbc a,a  
    sub h  
    ld h,a
prtdec0:        
    push bc
    ld c,0                      ; leading zeros flag = false
    ld de,-10000
    call prtdec1
    ld de,-1000
    call prtdec1
    ld de,-100
    call prtdec1
    ld e,-10
    call prtdec1
    inc c                       ; flag = true for at least digit
    ld e,-1
    call prtdec1
    pop bc
    ret
prtdec1:	     
    ld b,'0'-1
prtdec2:	    
    inc b
    add hl,de
    jr c,prtdec2
    sbc hl,de
    ld a,'0'
    cp b
    jr nz,prtdec3
    xor a
    or c
    ret z
    jr prtdec4
prtdec3:	    
    inc c
prtdec4:	    
    ld a,b
    jp putchar
                                 
prthex:                         ; display hl as a 16-bit number in hex.
    push bc                     ; preserve the IP
    ld a,h
    call prthex2
    ld a,l
    call prthex2
    pop bc
    ret
prthex2:		     
    ld	c,a
	rra 
	rra 
	rra 
	rra 
    call prthex3
    ld a,c
prthex3:		
    and	0x0F
	add	a,0x90
	daa
	adc	a,0x40
	daa
	jp putchar

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
    cp DQUOTE                      ; quote char
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
; the string should be immedaitely following the call
printStr:        
    ex (sp),hl		            ; swap			
    call prtstr		
    inc hl			            ; inc past NUL
    ex (sp),hl		            ; put it back	
    ret

init:
    ld ix,(vNext)
    ld iy,STACK
    ld hl,isysVars
    ld de,sysVars
    ld bc,8 * 2
    ldir
    ld hl,vars                          ; 52 vars LO HI 
    ld b,26*2                       
    xor a
init0:
    ld (hl),a
    inc hl
    djnz init0
    ret

start:
    ld sp,STACK		                    ; start of monty
    call init		                    ; setups
    call printStr		                ; prog count to stack, put code line 235 on stack then call print
    .cstr "Monty V0.0\r\n"

interpret:
    call prompt

    ld bc,0                             ; load bc with offset into TIB, decide char into tib or execute or control    
    ld (vTIBPtr),bc

interpret2:                             ; calc nesting (a macro might have changed it)
    ld e,0                              ; initilize nesting value
    push bc                             ; save offset into TIB, 
                                        ; bc is also the count of chars in TIB
    ld hl,TIB                           ; hl is start of TIB
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
    cp $0                       ; is it end of string? NUL end of string
                                ; ???? NEEDED?
    jr z,interpret8
    cp '\r'                     ; carriage return? ascii 13
    jr z,interpret7		        ; if anything else its macro/control 

                                ; macro       
;  ld (vTIBPtr),bc
;  ld hl,ctrlCodes
;  add a,l			            ; look up key of macros
;  ld l,a
;  ld e,(hl)
;  ld a,e
;  or a
;  jr z,macro1
;  ld d,msb(macros)
;  push de
;  call call		            ; monty exec_ operation and jump to it
;  db DC1,0
; macro1:
;  ld bc,(vTIBPtr)
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
    ld (vTIBPtr),bc
    ld bc,TIB                   ; Instructions stored on heap at address HERE, 
                                ; we pressed enter
    dec bc

next:        
    inc bc                      ; Increment the IP
    ld a,(bc)                   ; Get the next character and dispatch
    cp " "                      ; whitespace?
    jr z,next                   ; space? ignore
    jr c,next1
    ld l,a                      ; index into table
    ld h,msb(opcodesBase)       ; start address of jump table    
    ld l,(hl)                   ; get low jump address
    ld h,msb(page4)             ; Load h with the 1st page address
    jp (hl)                     ; Jump to routine
next1:
    cp NUL                      ; end of input string?
    jr z,exit_
    jp interpret                ; no, other whitespace, macros?
exit_:
    ld hl,bc
    jp (hl)

error:
    call printStr		        
    .cstr "Error "
    call prtdec
    jp interpret
