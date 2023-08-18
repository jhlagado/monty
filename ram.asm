STKSIZE     equ     $100        ; Stack size
TIBSIZE     equ     $100	    ; 256 bytes
BUFSIZE     equ     $100	    ; 256 bytes, wraps

.align $100
            ds STKSIZE
STACK:

.align $100
TIB:        ds TIBSIZE          ; must be one page, lsb of vTIBPtr is length and wraps around

.align $100
BUFFER:     ds BUFSIZE          ; must be one page, lsb of vBufPtr is length and wraps around

.align $100
VARS:
            ds 26 * 2 * 4       ; 52 vars, 3 bytes, RST LO HI CHAR
restarts:

RST08:      ds 2                 
RST10:      ds 2                 
RST18:      ds 2                 
RST20:      ds 2                 
RST28:      ds 2                 
RST30:      ds 2                ; 
BAUD        ds 2                ; 
INTVEC:     ds 2                ; 
NMIVEC:     ds 2                ; 
GETCVEC:    ds 2                ;   
PUTCVEC:    ds 2                ;   

sysVars:

; initialised sys variables (preserved)

vTIBPtr:    ds 2                 
vBufPtr     ds 2                 
vHeapPtr:   ds 2                 
vRecurPtr:  ds 2                
vDataWidth: ds 1                 
vNumBase:   ds 1                    
vHexPrefix: ds 1 
vEcho:      ds 1 
vStrMode:   ds 1                
            ds 1 
            ds 1
            ds 1
            ds 1
            ds 1

; uninitialised sys variables (preserved)

vPointer    ds 2                ; 
vRemain:    ds 2                ; 

vSavedIP:   ds 2                ;
vSavedSP:   ds 2                ;
vSavedNext: ds 2                ;
vSavedBP:   ds 2                ;

; uninitialised sys variables (unpreserved)

vTemp1:     ds 2                ; 
vTemp2:     ds 2                ; 
vTemp3:     ds 2                ; 
            ds 2                ; 
tbPtr:      ds 2                ; reserved for tests

titleBuf:    ds 20

HEAP:         
