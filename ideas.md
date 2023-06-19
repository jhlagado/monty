# Ideas

reset

### print

.a should print an array
.n should print newline
.p should print formatted like printf
.k should print the stack

### macros

^C to break program
^Q to cold boot
^J edit previous line
^K edit next line
backspace
editing
list

### streams
map 
filter 
reduce

### Maybes

fixed number of returns? 
- No because a variable number allows array population
- Needs a change to arg_list data structure

### Notes:

() are reserved for args and are never nested

} must be used to end the block because it is used with the nesting algorithm to
find the end of the block. It can be combined with other chars but these are treated
differently while parsing commands

:} could use a jump to start of block rather than repushing block and rewinding IP, 
this could be used for a C style "continue" command, /a conditional "again"?
this matches /x exit

# Done

loop syntax

alt commands like \r should be /r  
comments should be // like C++

/ is better than \ because
- easier to test in ASM80
- frees up \ for ASCII commands like \n etc like C

-expressions for -- and ++, postdec and postinc- removed
/c should be /p for partial application
/z} should be :}
self pushing block, then /d could be simply : or go
:} or :|} means repeat a section in musical notation
need to deal with ambiguity }:} 
/b should be /c (char)
/w should be /n (number)
/b exit, if false exit loop 
how about ^ for go? use /x for xor, use /b for break
":" should be "^" for "go"
/r should be "^" for "go"
replace ";" with "," for discard (still makes sense in C)
replace /f with ;
replace (ab:c) with \ab:c
replace {: :} with ( )
/h should return the heap* system variable for getting and setting 
"," should check BP, not go negative into stack frame

