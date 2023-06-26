.align $100
; .org RAMSTART

            ds DSIZE
STACK:

.align $100

vars:
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

vDataWidth: ds 2                ; 
vTIBPtr:    ds 2                ; 
vBufPtr:    ds 2                ; 
vNext       ds 2                ; 
vHeapPtr:   ds 2                ; 

; uninitialised sys variables (preserved)

vPointer    ds 2                ; 
vRemain:    ds 2                ; 

vSavedIP:   ds 2                ;
vSavedSP:   ds 2                ;
vSavedBP:   ds 2                ;
vChecksum:  ds 2                ;

; uninitialised sys variables (unpreserved)

vTemp1:     ds 2                ; 
vTemp2:     ds 2                ; 
vLastDef:   ds 2                ; 
vHashStr:   ds 2                ; 
tbPtr:      ds 2                ; reserved for tests

.align $100
TIB:        ds TIBSIZE
BUF:        ds BUFSIZE

.align $100
pad:        ds $100

HEAP:         
