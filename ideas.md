# Ideas

# Unused symbols

```
#
```

# August
/max maximum
/min minimum

7 1 /max 9 /min

/mal memory allocate? 
/fre free from pointer
/fra free array
/mem amount of heap left

/buf instead of vB
/bf and /bf0 -- buf
/tb and /tb0 -- tib
/hp and /hp0

        -- are these needed now?

/ech for echo

/drp or ";" or /voi or /clr ?
";" sets SP to BP (i.e. iy)

_ab 16 bit symbol? a fast 16 bit hash would be better

"hello" /hsh


echo on/off ?

/fs should be just /sc for source
can connect to {,c}
could connect to {/in}

drop / from commands, assume lower case letters are commands
uppercase are global variables?
or a prefix like :

:a :X

etc similar to locals

$a

or #a #X and use : for arrays

[1 2 3] 1:

or even ;

[1 2 3] 1;

1. convertb to 3 letter command
2. prefix globals vars
3. drop the leading / from commands

variables can always be increased in number by using arrays

[0 0 0] :A =

:A
:A3#

:A
20 :A3;=

#A
20 #A 3; =

what about , ?

#A
20 #A 3, =

: or ; could be used for input

:c :s :



====

/mx maximum
/mn minimum
/bn between         value start end(exclusive) -- bool

/tk take

/ct count
/ta to array
/lt latest
/sq square root

/as /ak /ta array sink

[ 1 2 3 ] /as \i{%i 3 <} /ta


## done

/sel from an associative array

```
_B [_A {100} _B {200} _P {300}] /sel", 200
```

input
,   input number
,c  input char
,s  input string

use ** to spread an array onto stack?

[ 1 2 [ 3 4 5 ] ** 6 7 ]

[ 1 2 3 4 5 6 7 ]

DO we really need ?? as opposed to just ?, use 0 for null 
/hb should be /hx
/db should be /dc

string comparison
/al - array length
/sl - string length
/as - might be the array size in bytes?
/sb string mode begin
/se string mode end

## on hold

need to rework looping

/db - decimal base
/bb - for bye bye
/bm - byte mode
/wm - word mode
/hb - hex base

all strings are null terminated

# July 

all print statements should have the option to print to the output 
or to a string. It should probably be limited to a string of 255 chars
other wise it might go rogue and wipe memory.

All print routines either a print char routine or an append routine
the routine could use a flag to terminate the caller.
Some print routines use other print routines e.g. arrays
instead og a Buffer, just use the first available location on heap. 
this could create a string if desired

/ps // print to string
/pt // print to terminal
/sb string begin
/se string end, pushes start address, null terminates
/sb `hello ` 1. 2. 3. /se

same as "hello 123"

/vB and /vb keep track of the printing on the heap. 
If /vb - /vB > 1000 then terminate

formatting of primitive might happen in BAF or PAD and get 
copied to heap?
If print to terminal then immediately print pad

if writing to string, 

set vB to vHeap + 1000
set vb to vHeap

use vB as the upper limit for vb

if printing to terminal then after operation, 
"next" prints everything from vHeap to vb and then sets vb to vHeap
terminates if vb >= vB

if printing to string, vb is incremented up to limit vB
when /se occurs: 
vHeap is pushed on heap
NUL is written
vHeap moved to vb
vB = vHeap + 1000

vB might be unneccesary because it is alway 1000 more than vHeap

- string compare

- print length should be stored in a variable
this could be used in formatting


----------------------------------------------------------------

512 7.#

....512
    
where spaces are shown here as dots     
or 

7 /pd 512 .

....512

sets the padding for numeric printing
only needed for decimals

0 /pd is padding off

- /ss string size
- loops need to clean up better
- bring back previous line to edit

- printf style formatting

e.g. 

[2 5] `it is %n for %n dollars` .p

if it could count how many interpolations there were, 
the array could be eliminated and the stack used directly

e.g.

2 5 `it is %n for %n dollars` .p

need to count the %'s and deal with escaping such as \%

where 

// %d is decimal
// %h is hex  ???? no use base

%n is number in current base
%c is char
%s is string
%a is array ... based on current data width

---- done July
- array src
- string src
- iter src

- filter, scan
- eliminate ; from functions by making param block parse bodies
- := declaration operator

----

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

