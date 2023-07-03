# Ideas

type annotation
polymorphic print
any value can be a pointer. If it is not a pointer then it contains a number value
the only way to tell if a stack item or a variable value is a pointer is to follow it 
and see if it points to a known value type.
Right now this is simply a byte where the lower nybble contains a type code. 
The upper nybble could contain a magic number.
If the probability of a false positive is seen as too high, the previous byte could also be magic
This polymorphism is solely for printing so the chances of this running away are not high
Probably the worst case would be a string printing until the first null is encountered.
Also added to this is a NumBase variable which is also used for printing
NumBase does not affect parsing because it uses prefixes


every variable and every heap allocated item can have a type annotation. 
Items on the stack cannot

variables

number 
pointer

heap

number
char
string
pointer
array
reference
block
arglist
function

Allocation always allows this
type is always at -1 byte before pointer address

the highest bit might indicate auto-executable for blocks and fuctions

the main reason for these type tags is so they can be printed. 
printing is a type of execution. Might be used with NEXT


reset

### print

.a should print an array
.p should print formatted like printf

### macros

^C to break program
^Q to cold boot
^J edit previous line
^K edit next line
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

() are reserved for loops

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
.k should print the stack

