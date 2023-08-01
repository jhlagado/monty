# Monty Language

_I'd like to have an argument, please._

## What is Monty?

Monty is a minimalist RPN based language "concatenative" interpreter but one
which aims at readability and use of use. It is written for the Z80 miroprocessor
and takes about 4K.

Unlike other interpreters, Monty does not use obscure symbols. Instead it uses
well-known symbols and conventions to do expected things. Sometimes an operations
is made of two symbols like <= which means "less than or equal to". Where possible
Monty follows the conventions laid out in the C programming language so the meanings
of Monty operations should be recognisable to programmers of other languages.

Monty has functions and you pass values to them by name just like in the C language.
Unlike Forth, There are no built in stack manipulation words in Monty. There is
still a stack, just one stack, but the user doesn't normally care about it.

Example, a function to square a value a

```
\a{ %a %a * } ;
```

The function is a value like any other and you can store it in a variable and call
it with ^. Here I'm storing it in a variable F

```
\a{ %a %a * } ; F=
```

And calling it

```
10 F^ .

-> 100
```

There are 52 variables A..Z, a..z and they are simpler to use. Upper and lower case
variable names can be used to store anything, functions, numbers, arrays etc.

To put something in a variable, use = to assign.

Store 10 in x

```
x := 10 ;
```

To access a value in a variable, just use the name.

```
3 x + .

-> 13
```

You can put any code inside { } block which means "execute this later". You execute
it with ^. Here's an immediately executed block of code.

```
{ 1 2 + . }^

->3
```

Here's an "if" condition

```
3 2 > { "hello" .s } ?
```

If 3 is greater than 2 then print hello

- .s means print string
- ? Means if the condition is true then execute the block.
- if..else is done with ??

```
3 2 > { "greater" .s } { "less than" .s } ??
```

Loops are infinite and are represented with ( )
You run them with ^
You can terminate them with /br which will break the loop if a condition is false.
Counting to 10

```
1 i= ( i . i 10 <= /br )^
> 1 2 3 4 5 6 7 8 9 10
```

There are other commands in Monty which do not use symbols. These use a / followed
by one or two letters. Example, to `xor` two values:

```
$55 $FF /x .
> 255
```

To show as hex use /h and /d for decimal

```
255 /h .
> $FF
```

Arrays are simple and can be byte or word sized.
Create an array and store in A

```
[ 10 20 30 ] A=
```

To access the second element (index 1) of this array

```
A 1# .
>20
```

### Commands

#### Conditional code

```
?       if..then
??      if..then..else
```

#### Input & Output

```
.       print number
.c      print character
.s      print string
.a      print array
``      print literal string
/k      key
/in     input
/o      output
```

#### Loops

```
()      loop
/br     break
```

#### Logical

```
!       not
!=      not equal
==      equal
>       greater than
>=      greater than or equal
<       less than
<=      less than or equal
&       bitwise and
|       bitwise or
/x      bitwise exclusive or
/t      true
/f      false
```

#### Arithmetic

```
/       division
*       multiplication
+       addition
-       subtraction
/ab     absolute
/re     remainder
```

#### Arrays

```
[]      array delaration
#       array index
/ai     array iterator
/al     array length
/as     array size
```

#### Functions

```
\       function begin
{}      code block
%a..%z  argument reference
/fs     function source
/rc     recur
/qt     quit
```

#### Numbers

```
$       hex number prefix
/wm     word mode
/bm     byte mode
/db     decimal base
/hb     hexadecimal base
```

#### Strings

```
'       string
~       literal character
/si     string iterator
/sl     string length
/sb     string begin
/se     string end
/ss     string size
```

#### Streams

```
/rg     range source
/ft     filter
/fe     for each
/fd     fold
/mp     map
```

#### Variables

```
=       assign
+=      increment var by
++      increment var by 1
-=      decrement var by
--      decrement var by 1
+=      multiply var by
/=      divide var by
&=      multiply var by
|=      multiply var by
/x=      multiply var by


A..Z    global variable reference
a..z    global variable reference
:=      declaration begin
;       declaration end
/ad     addr of
/vb     text output buffer variable
/vh     heap pointer variable
/vt     text input buffer variable
/vB     text output buffer start
/vH     heap start
/vT     text input buffer start
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
