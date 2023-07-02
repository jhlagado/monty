TEC_1 EQU 1
RC2014 EQU 0

EXTENDED EQU 0

.if RC2014

; Configuration for RC2014

ROMSTART    EQU $8000
RAMSTART    EQU $8800
LOADER EQU 0
BITBANG EQU 0

.endif

.if TEC_1
        
; Configuration for TEC-1
LOADER EQU 0
BITBANG EQU 0
        
ROMSTART    EQU $0000
RAMSTART    EQU $2000               ; TODO: set this to TEC 1G 
ROMSIZE     EQU $2000
RAMSIZE     EQU $2000

;TEC-1D SC 8k rom/ram 
; ROMSTART .equ $0000
; RAMSTART .equ $2000
; ROMSIZE  .equ 8192
; RAMSIZE  .equ 8192

.endif
