# Monty Language

_I'd like to have an argument, please._

## What is Monty?

Monty is a minimalist RPN based language "concatenative" interpreter but one
which aims at readability and use of use. It is written for the Z80 microprocessor
and takes about 4K.

Unlike other interpreters, Monty does not use obscure symbols. Instead it uses
well-known symbols and conventions to do expected things. Sometimes an operations
is made of two symbols like <= which means "less than or equal to". Where possible
Monty follows the conventions laid out in the C programming language so the meanings
of Monty operations should be recognisable to programmers of other languages.

[Monty tutorial](tutorial.md)

### Commands

#### Conditional code

```
?       if..then..else                  bool block1* block2* ? --
```

#### Input & Output

```
.       print number                    num --
.c      print character                 char --
.s      print string                    str* --
.a      print array                     arr* --
``      print literal string            --
,       input number                    -- num
,c      input char                      -- char
,s      input string                    -- str*

/in     input                           -- num
/out    output                        num port --
```

#### Loops

```
()      loop                            --
/whi    while                           bool --
```

#### Logical

```
!       not                             num -- num
!=      not equal                       num num -- bool
==      equal                           num num -- bool
>       greater than                    num num -- bool
>=      greater than or equal           num num -- bool
<       less than                       num num -- bool
<=      less than or equal              num num -- bool
&       bitwise and                     num num -- num
|       bitwise or                      num num -- num
~       bitwise invert                  num -- num
/x      bitwise exclusive or            num num -- num
/tru      true                          -- bool
/fal      false                         -- bool
```

#### Arithmetic

```
/       division                        num num -- num
*       multiplication                  num num -- num
+       addition                        num num -- num
-       subtraction                     num num -- num
/abs    absolute                        num -- num
/rem    remainder                       -- num
```

#### Arrays

```
[]      array delaration                -- arr*
;       array index                     arr* num -- num
**      array spread                    arr* -- item1 item2 ... itemN
/ai     array iterator                  arr* -- src*
/aln     array length                   arr* -- num
/as     array size                      arr* -- num
```

#### Functions

```
\       function begin
{}      code block
%a..%z  argument reference
/fs     function source                 func* -- src*
/rc     recur                           --
/ret    return                          bool --
```

#### Numbers

```
$       hex number prefix
/wrd    word mode                       --
/byt    byte mode                       --
/dec    decimal base                    --
/hex    hexadecimal base                --
```

#### Strings

```
''      string                          -- str*
_       literal character               -- char
/si     string iterator                 str* -- src*
/sl     string length                   str* -- num
/ss     string size                     str* -- num
/sb     string begin
/end     string end                      -- str*
/sc     string compare                  str* str* -- bool
```

#### Streams

```
/rg     range source                    num num num -- src*
/ft     filter                          func* -- src*
/for    for each                        func* -- src*
/fd     fold                            val func* -- src*
/map    map                             func* -- src*
```

#### Variables

```
=       assign                          val --
+=      increment var by                num var --
++      increment var by 1              var --
-=      decrement var by                num var --
--      decrement var by 1              var --
+=      multiply var by                 num var --
/=      divide var by                   num var --
&=      bitwise and var by              num var --
|=      bitwise or var by               num var --
/x=     bitwise xor var by              num var --
~=      bitwise invert var              var --

A..Z    global variable reference       -- val
a..z    global variable reference       -- val
/adr     addr of                        char -- *
```

#### Misc

```
^       execute
/bb     cold start
```

### Licence & credits

```
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
```

### Types

```
            0   reserved
TNUMBER     1   number
TSTRING     2   string
TPOINTER    3   pointer
TARRAY      4   array
TBLOCK      5   block
TLAMBDA     6   lambda function
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
