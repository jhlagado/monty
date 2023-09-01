# Monty Language

_I'd like to have an argument, please._

## What is Monty?

Monty is a minimalist character-based interpreter but one which aims at fast performance,
readability and ease of use. It is written for the Z80 microprocessor and is 5K.

Unlike other character-based interpreters, Monty does not use obscure symbols.
Instead it uses well-known conventions to do expected things. Sometimes an operation
is made of two symbols such as <= which means "less than or equal to". Wherever possible,
Monty follows the conventions used by the C programming language so the meanings of
Monty operations should be fairly recognisable to programmers of other languages.

## Reverse Polish Notation (RPN)

RPN is a [concatenative](https://concatenative.org/wiki/view/Concatenative%20language)
way of writing expressions in which the operators come after their operands.
This makes it very easy to evaluate expressions, since the operands are already on the stack.

Here is an example of a simple Monty program that uses RPN:

```
10 20 + .
```

This program pushes the numbers `10` and `20` are operands which are followed by an
operator `+` which adds the two operands together. The result becomes operand for
the `.` operator which prints the sum.

## Numbers in Monty

Monty on the Z80 uses 16-bit integers to represent numbers. A valid (but not very
interesting) Monty program can be simply a sequence of numbers. Nothing will happen
to them though until the program encounters an operator.

There are two main types of numbers in Monty: decimal numbers and hexadecimal numbers.

### Decimal numbers

Decimal numbers are represented in Monty in the same way that they are represented
in most other programming languages. For example, the number `12345` is represented
as `12345`. A negative number is preceded by a `-` as in `-786`.

### Hexadecimal numbers

Hexadecimal numbers are represented in Monty using the uppercase letters `A` to `F`
to represent the digits `10` to `15`. Hexadecimal numbers are prefixed with a `$`.
So for example, the hexadecimal number `1F3A` is represented as `$1F3A`.
Unlike decimal numbers, hexadecimal numbers are assumed to be positive in Monty.

### Formatting numbers

Monty provides commands for formatting hexadecimal and decimal numbers. The print
operator `.` prints numbers in the current base. To switch the base to hexadecimal
use the command `/hex` before using the `.` operator. To switch back to formatting
in decimal use the command /dec.

## Basic arithmetic operations

```
5 4 * .
```

In this program the numbers `5` and `4` are operands to the operator `*` which
multiplies them together. The `.` operator prints the result of the
multiplication.

```
10 20 - .
```

This program subtracts `20` from `10` which results in the negative value `-10`
The `.` operator prints the difference.

```
5 4 / .
```

This program divides 5 with 4 prints their quotient. Monty for the Z80 uses
16-bit integers. The remainder of the last division operation can accessed using
the `/rem` operator.

```
/rem .
```

## Variables and Variable Assignment

Variables are named locations in memory that can store data. Monty has a limited
number of global variables which have single letter names. In Monty a variable can
be referred to by a singer letter from `a` to `z` or from `A` to `Z` so there are 52
globals in Monty. Global variables can be used to store numbers, strings, arrays, blocks, functions etc.

To assign the value `10` to the global variable `x` use the `=`.

```
10 x =
```

In this example, the number `3` is assigned to the variable `x`

To access a value in a variable `x`, simply refer to it.
This code adds `3` to the value stored in variable `x` and then prints it.

```
3 x + .
```

The following code assigns the hexadecimal number `$3FFF` to variable `A`
The second line fetches the value stored in `A` and prints it.

```
$3FFF A =
A .
```

In this longer example, the number `10` is stored in `a` and the number `20` is
stored in `b`. The values in these two variables are then added together and the answer
`30` is stored in `Z`. Finally `Z` is printed.

```
10 a =
20 b =
a b + Z =
Z .
```

## Arithmetic assignment Operators

Monty has a set of arithmetic assignment operators that can be used to combine an
assignment with an arithmetic operation. These operators are:

- `+=` adds the left operand to the right operand and assigns the result to the right operand.
- `-=` subtracts the left operand from the right operand and assigns the result to the right operand.
- `*=` multiplies the left operand by the right operand and assigns the result to the right operand.
- `/=` divides the left operand into the right operand and assigns the result to the right operand.

For example, the following code:

```
10 x =
5 x +=
```

is equivalent to the following code:

```
10 x =
5 x + x =
```

Here are some more examples of Monty's assignment operators:

```
10 x =
5 x -=          // x is now 5

10 y =
2 y *=          // y is now 20

10 z =
2 z /=          // z is now 5
```

## Arrays

Monty arrays are a type of data structure that can be used to store a collection of elements. Arrays are indexed, which means that each element in the array has a unique number associated with it. This number is called the index of the element.
In Monty, array indexes start at 0

To create a Monty array, you can use the following syntax:

_[ element1 element2 ... ]_

for example

```
[ 1 2 3 ]
```

Arrays can be assigned to variables just like number values

```
[ 1 2 3 ] A =
```

To access an element in an array, you can use the following syntax:

_array index ";"_

For example, the following code would access the second element in the array array:

```
A 1 ; .
```

You can find the length of an array with the /aln operator. For example, the following code would print the number of elements
in the array array:

```
A /al .
```

Just as individual values can be printed with the `.` operator, arrays can be printed using the `.a` operator.

```
A .a
```

prints `[ 1 2 3 ]` to the output

## Data width

Monty can work in two modes: _byte mode_ and _word mode_. In byte mode, all values are assumed to be 8 bits, while in
word mode, all values are assumed to be 16 bits. The user can change the mode to byte mode by using the command /byt.
The user can change to word mode with the command /wrd.

The default mode for Monty is word mode. This means that if the user does not specify a mode, all values will be assumed to be
16 bits.

When Monty is in word mode, the following rules apply:

- All values are stored as 16-bit integers.
- All operations are performed on 16-bit integers.

When Monty is in byte mode, the following rules apply:

- All values are stored as 8-bit integers.
- All operations are performed on 8-bit integers.

This difference most relevant during memory access and working with arrays.

For example when an array is defined while in byte mode then all elements are assumed to be 8 bit and that indexes refer to
consecutive bytes.

```
/byt [1 2 3] A=
```

Also array length is measured in bytes.

```
A /aln .
```

This would print `3` bytes

If an array is defined while in word mode then all the elements are assumed to be 16 bits and that indexes refer to
consecutive 16 bit words.

```
/wrd [1 2 3] A=
```

Also array length is measured in bytes.

```
A /aln .
```

This would print `3` words (6 bytes)

## Characters

Printable ASCII characters can be defined literally in Monty by using the `_` prefix

For example to define a byte mode array of characters, you could do this

```
/byt [ _h _e _l _l _o ] A=
```

Characters are simply numbers but they can be printed as ASCII values using the `.c` operator

```
A 0 # .c
```

prints `h`

## Strings

Monty allows null-terminated strings to be defined by surrounding the string with `'` characters.

```
'hello there!' S =
```

Strings can be prints with the `.s` operator

```
S .s
```

prints out `hello there!`

the length of a string can be got with the /sln operator

```
A /sln .
```

This prints the length of `12`

Strings can also be compared for equality with the `/scp` operator

```
'hello' 'Hello' /scp .
```

prints `0` (for false)

### Printing values

Monty has a number of ways of printing to the output.

`<value> .` prints a value as a number. This command is affected by /hex /dc /byt /wrd  
`<value> .c` prints a value as an ASCII character
`<value> .s` prints a value as a pointer to a null terminated string
`<value> .a` prints a value as a pointer to an array. This command is affected by /hex /dc /byt /wrd

Additionally Monty allows the user to easily print literal text by using \` quotes.

For example

```
100 x =
`The value of x is ` x .
```

prints `The value of x is 100`

## String builder

Anything that can be written to the screen can be captured and turned into a string
by using Monty's string builder.

```
234 r =
123 g =
89  b =
/sbb `red: ` r . `green: ` g . `blue: ` b . /sbe T =
T .s
```

Stores `red: 234 green: 123 blue: 89` as a string in variable T.
It then prints the string in T

## Logical operators

Monty uses numbers to define boolean values.

- false is represent by the number `0`
- true is any non-zero value, however the most useful representation is `-1` ($FFFF)

Any non-false value can be inverted to `0` (false) using the `!` operator, a false value is inverted to `-1` (true)

```
3 ! .
```

prints `0`

```
0 ! .
```

prints `-1`

Monty has a set of bitwise logical operators that can be used to manipulate bits. These operators are:

`&` performs a bitwise AND operation on the two operands.
`|` performs a bitwise OR operation on the two operands.
`\x` performs a bitwise XOR operation on the two operands.
`~` performs a bitwise NOT operation on the operand.
`<<` shifts the bits of the operand to the left by the specified number of positions.
`>>` shifts the bits of the operand to the right by the specified number of positions.

The bitwise logical operators can be used to perform a variety of operations on bits, such as:

- Checking if a bit is set or unset.
- Setting or clearing a bit.
- Flipping a bit.
- Counting the number of set bits in a number.

Here is an example of how to use the bitwise logical operators in Monty:

Check if the first bit of the number 10 is set

```
10 & 1 .
```

this will print `1`

Set the fourth bit of the number 10

```
1 3 << 1 | /hex .
```

prints $0009

Flip the third bit of the number 10

```
1 2 << $0F /xor /hex .
```

prints $000B

## Code blocks

You can put any code inside { } block which tells Monty to "execute this later".

Code blocks can be stored for later or immediately executed.

Storing a code block in the variable `Z`.

```
{`hello` 1. 2. 3.} Z =
```

Running the code block stored in `Z` by using the `^` (execute) operator

```
Z^
```

will print out.

```
hello 1 2 3
```

Immediately executing a code block

```
{`hello` 4. 5. 6.}^
```

This will print out.

```
hello 4 5 6
```

## Conditional code

Code blocks are useful when it comes to conditional code in Monty.

The syntax for a Monty IF-THEN-ELSE or "if...else" operator in Monty is:

```
condition code-block-then code-block-else ?
```

If the condition is true, then code-block-then is evaluated and its value is returned.
Otherwise, code-block-else is evaluated and its value is returned.

Here is an example of a "if...else" operator in Monty:

```
10 x =
20 y =

x y > { 'x is greater than y' } { 'y is greater than x' } ? z =

z .s
```

In this example, the variable x is assigned the value 10 and the variable y is assigned the value 20. The "if...else" operator then checks to see if x is greater than y. If it is, then the string "x is greater than y" is returned. Otherwise, the string "y is greater than x" is returned. The value of the "if...else" operator is then assigned to the variable z. Finally, the value of z is printed to the console.

Here is another example of the "if...else" operator in Monty. This time, instead of creating a string just to print it, the following
code conditionally prints text straight to the console.

```
18 a =

`This person` a 18 >= {`can`} {`cannot`} ? `vote`
```

In this example, the variable a is assigned the value 18. The "if...else" operator
then checks to see if age is greater than or equal to the voting age of 18. If it is,
then the text "can" is printed to the console. Otherwise, the string "cannot" is printed to the console.

Monty can also use the "select" operator `/sel` to choose between multiple cases.

```
key array-of-pairs /sel
```

where a "pair" is a number followed by a block. For example:

```
100 {`one hundred`}
```

Here is an example that selects a number 0, 1, 2 or 3 and prints its text name.
The array of pairs is stored in `A`
Then we select `2` from the array and execute the corresponding block

```
[0 {`zero`} 1 {`one`} 2 {`two`} 3 {`three`}] A =
2 A /sel
```

In the following example, we have an array of character-block pairs.
We select the block that matches the associated character.

In this example with select "B" which returns the number 2.

```
_B [_A {1} _B {2} _C {3}] /sel
```

## Functions in Monty

In Monty functions are anonymous and can be called directly or assigned to variables.
Functions are first-class citizens. They are a powerful feature of Monty that can be used to
simplify code and make it more concise.

### Basic Function Syntax

In Monty, functions are denoted by the `\` symbol followed by one or more arguments
(single lowercase letters) and a `{` symbol to indicate the beginning of the
function expression. The body of the function is written using Reverse Polish Notation (RPN),
where `%` is used to reference the function's arguments.

A basic function with a single argument is represented as follows:

```
\a{ %a . }
```

This function takes a single argument `a` and prints its value using the `.` operator.

Example: a function to square a value a

```
\a{ %a %a * }
```

### Function with Multiple Arguments

You can also define functions with multiple arguments. For example:

```
\ab{ %a %b + . }
```

This function takes two arguments `a` and `b`, adds them together using the `+` operator,
and then prints the result using `.`.

### Calling functions

Functions are called with the ^ operator

```
30 20 \ab{ %a %b * } ^ .
```

This code passes the numbers `30` and `20` to a function which multiplies them and returns
the result which is then printed.

### Assigning Functions to Variables

In Monty, you can assign functions to variables just like any other value.
Variables in Monty are limited to a single uppercase or lowercase letter. To
assign a function to a variable, use the `=` operator.

Let's see some examples:

Here's a function to print a number between after a `$` symbol and storing t in variable `A`

```
\a{ `$` %a . } A =
```

And calling it:

```
100 A^
```

The `100` is passed to the function as argument `a`. The function first prints `$` followed by `1001

Here's a function to square two numbers. The function is stored in variable S

```
\a{ %a %a * } S =
```

Calling it:

```
4 S^ .
```

The number `4` is passed to the function S which squares the value and then prints it.

```
\ab{ %a %b + } T =
```

Here, we assigned the functions to variables `A` and `B` respectively.

`A` represents the function that takes a single argument and prints it, while `B`
represents the function that takes two arguments, adds them, and prints the result.

### Using Functions

Once you've assigned functions to variables, you can use them in your Monty code.
To execute a function and pass arguments to it, use the `^` operator. The function's
body will be executed, and the result, if any, will be printed.

Example:

```
10 A^       // prints 10
3 7 B^      // prints 10, the sum of 3 and 7
```

In the first line, we execute the function stored in variable `A` with the argument `10`,
which prints `10`. In the second line, we execute the function stored in variable `B` with
arguments `3` and `7`, which results in `10` being printed (the sum of the two arguments).

### Higher-Order Functions

Monty allows you to create higher-order functions that take other functions (functions)
as arguments or return them as results. This enables powerful and flexible programming.

For example, consider the following function:

```
\ab{ %a %b^ }
```

This function takes two arguments - a function a and an argument b. It then applies
the function `a` to the argument `b` using the `^` operator.

### Lexical Scoping

In Monty, variables have lexical scoping, meaning they are only accessible within
the scope they are defined in. If a variable is defined inside a function, it is
accessible only within that function's body.

Example:

```
\x{ \y{ %x %y + . } . }
```

In this example, the inner function has access to the argument x of the outer function,
but it cannot access any variables outside its scope.

Functions can also contain local variables. Local variables can be declared in
functions by following the arguments with a colon and then one or more local variables.
The syntax for declaring a local variable in a function is as follows:

```
\args:locals{ body }
```

For example, the following function contains a local variable c:

```
\ab:c{ %a %c = %b %c . }
```

This function takes two arguments, `a` and `b`, and a local variable, `c`.
The body of the function first assigns the value of `a` plus `b` to `c`.
Then, it prints the value of `c`.

Local variables in functions are only accessible within the scope of the function.
This means that they cannot be accessed from outside the function.

Here are some more examples of local variables in functions:

A function with a single local variable

```
\a:c{ 10 %c = %c . }
```

The function takes a single argument, `a`, and a local variable, `c`. The body of
the function first assigns the value of `10` to `c`. Then, it prints the value of `c`.

A function with two local variables

```
\ab:c{ %a %c = %b + %c . }
```

The function takes two arguments, `a` and `b`, and a local variable, `c`. The body
of the function first assigns the value of `a` plus `b` to `c`. Then, it prints the
value of `c`.

As you can see, local variables can be used to store temporary values in functions.
This can make your code more concise and easier to read.

Here are some additional things to keep in mind about local variables in functions:

- Local variables must be declared before they are used.
- Local variables are only accessible within the scope of the function.
- Local variables can be overwritten within the scope of the function.

## Loops

A loop in Monty is a represented by a pair of parentheses `(` and `)` which
surround the code to be repeated.

```
( `hello` )
```

However, while the above code declares a loop, it does nothing more. to run the
loop we need to use the execute operator `^`. This works the same as executing a
code block or a function.

```
( `hello` )^
```

The above code prints "hello" to the terminal over and over forever. By default
loops in Monty are endless loops which can
only be stopped by resetting the Z80 CPU (this will cause a warm reboot in monty).

```
( `hello` )^
```

To control how many iterations a loop does we need to use the "while" operator `/whi`

```
0 i = (i 10 < /whi i . i++ )^
```

The above code initializes the variable `i` to zero and enters the loop.
It compares i to 10, the result is passed to `/whi`
If i is greater than or equal to 10 then `/whi` terminates the loop
otherwise it prints the value of `i` and then increments it
This code continues to repeat until 10 is reached and terminates

```
0 1 2 3 4 5 6 7 8 9
```

The `/whi` operator can appear anywhere in the loop body.

If it appears as the first thing in the loop body then the loop acts like a "while" loop
i.e. a loop that executes zero or more times, depending on its expression

If it appears later inn the body then the loop behaves like a "do...while" loop
i.e. a loop which executes its body at least once

Here is a "do...while" style loop

```
0 i = ( i . i++ i 10 < /whi )^
```

### Commands

#### Conditional code

```
?       if..then..else                  bool blk1* blk2* ? --
/sel    select                          key array* --
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
/out    output                          num port --
/ech    echo                            bool --
```

#### Terminal

/cur cursor hide/show bool --
/cll clear line type -- type 0: to end 1: to start 2: entire line
/cls clear screen --
/cmv cursor move x dir -- dir: 0:up 1:down 2:forward 3:back
/cgo cursor go x y --

#### Loops

```
()      loop                            --
/whi    while                           bool --
```

#### Arithmetic

```
/       division                        num num -- num
*       multiplication                  num num -- num
+       addition                        num num -- num
-       subtraction                     num num -- num
/abs    absolute                        num -- num
/rem    remainder                       -- num
/max    maximum                         num num -- num
/min    minimum                         num num -- num
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
/xor    bitwise exclusive or            num num -- num
/tru    true                            -- bool
/fal    false                           -- bool
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
*=      multiplication                  num var --
&=      bitwise and var by              num var --
|=      bitwise or var by               num var --
/xor=   bitwise xor var by              num var --
~=      bitwise invert var              var --

A..Z    global variable reference       -- val
a..z    global variable reference       -- val
/adr     addr of                        char -- *
```

#### Arrays

```
[]      array declaration               -- arr*
;       array index                     arr* num -- num
**      array spread                    arr* -- item1 item2 ... itemN
/aln    array length                    arr* -- num
```

#### Functions

```
\       function begin
{}      code block
%a..%z  argument reference
/rec    recur                           --
/ret    early return from function      bool --
/voi    void function return            --
```

#### Streams

```
/ait    array iterator                  arr* -- src*
/for    for each                        func* -- src*
/ftr    filter                          src* func* -- src*
/src    source                          blk* -- src*
/map    map                             src* func* -- src*
/rng    range src                       start end step -- src*
/scn    scan stream                     src* init rfunc* -- src*
/sit    string iterator                 str* -- src*
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
/sbb    string builder begin            --
/sbe    string builder end              -- str*
/scp    string compare                  str* str* -- bool
/sln    string length                   str* -- num
```

#### Misc

```
^       execute                         blk* | func* | loop* --
/bye    cold start                      --
/var    pointer to system vars          -- *
/alc    memory allocate                 num -- *
/fre    free memory                     * --
/fra    free memory array               * --

```

### License & credits

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
