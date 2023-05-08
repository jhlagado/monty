.ORG RAMSTART

            ds DSIZE
STACK:

tbPtr:      ds 2                ; reserved for tests

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

vFrac:      ds 2                ; initialised sys variables
vDataWidth: ds 2                ; 
vTIBPtr:    ds 2                ; 
vPointer    ds 2                ; 
vLastDef:   ds 2                ; 
vHashStr:   ds 2                ; 
vNext       ds 2                ; 
vHeapPtr:   ds 2                ; 

vTemp1:     ds 2                ; uninitialised sys variables

.align $100
TIB:        ds TIBSIZE

.align $100
pad:        ds $100

.align $100
hashSlots:  ds $100

.align $100
hashWords:  ds $100

HEAP:         
