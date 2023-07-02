; ************************************\*************************************
;
; Monty programming language for the Z80
;
; by John Hardy 2022
;
; Incorporating code from the MINT project by Ken Boak and Craig Jones.
;
; GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007
;
; see the LICENSE file in this repo for more information
;
; **************************************\***************************************

### Types

```
            0   reserved
TNUMBER     1   number
TSTRING     2   string
TPOINTER    3   pointer
TARRAY      4   array
TBLOCK      5   block
TFUNCTION   6   function
TARGLIST    7   arglist
            8   reserved
            9   reserved
```

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



