Introduction

What is Monty?

Monty syntax and conventions

* Reverse Polish Notation (RPN)
* Numbers in Monty 
    - Decimal numbers
    - Hexadecimal numbers
    - Formatting numbers
* Basic arithmetic operations
* Variables and variable assignment
* Input and output functions
* Understanding Monty's stack

Control Flow in Monty

* Conditionals (? and ??)
* Loops (() and /br)
* Higher-order functions and functions (, {}, %, ^)

Data Structures

* Arrays ([] and #)
* Strings (', _, /si, /sl, /sb, /se, /ss)
* Basic operations on arrays and strings

Monty's Built-in Commands

* Logical operations (!, !=, ==, >, >=, <, <=, &, |, /x, /t, /f)
* Arithmetic operations (/, *, +, -, /ab, /re)
* Variables and variable operations (=, +=, ++, --, /ad, /vb, /vh, /vt, /vB, /vH, /vT, A..Z, a..z, :=, ;)
* Functions and function expressions (, {}, %, ^, /fs, /rc, /qt)

Advanced Monty Programming

* Lexical scoping and variable visibility
* Working with multiple files and modules
* Error handling and debugging techniques
* Best practices for writing efficient Monty code
* Using Monty for real-world applications

Examples and Practical Projects

* Implementing algorithms and data structures in Monty
* Developing simple applications and utilities
* Creating reusable Monty libraries

Conclusion

* Summary of Monty's features and capabilities
* Final thoughts and next steps in mastering Monty
* Additional resources for further learning

## What is Monty?

Monty is a minimalist character-based interpreter but one which aims at readability 
and ease of use. It is written for the Z80 miroprocessor and is only 4K.

Unlike other character-based interpreters, Monty does not use obscure symbols. 
Instead it uses well-known conventions to do expected things. Sometimes operations
are made of two symbols such as <= which means "less than or equal to". Wherever possible,
Monty follows the symbols and conventions used in the C programming language so 
the meanings of Monty operations should be fairly recognisable to programmers of many 
other languages.

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
use the command `/hx` before using the `.` operator. To switch back to formatting 
in decimal use the command /dc.

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

This program subtracts `20` from `10` which results in the neagtive value `-10`
The `.` operator prints the difference.

```
5 4 / .
```

This program divides 5 with 4 prints their quotient. Monty for the Z80 uses 
16-bit integers. The remainder of the last division operation can accessed using 
the `/re` operator.

```
/re .
```

## Variables and Variable Assignment

Variables are named locations in memory that can store data. Monty has a limited
number of global variables which have single letter names. In Monty a variable can
be referred to by a letter from `a` to `z` and `A` to `Z`.

To assign a value to a number:

```
3 x =
```

In this example, the number `3` is assigned to the variable `x`

```
$3FFF A =
```

This assigned the hexadecimal number `$3FFF` to variable `A`

To access the value stored in a variable, simply refer to by letter.

```
1 x + .
```

This program adds the number `1` to the value stored in `x` earlier. The 
result is the number `4` which is then printed.

```
10 a =
20 b =
a b + Z =
Z .
```

In this longer example, the number `10` is stored in `a` and the number `20` is 
stored in `b`. The values in these two variables are then added and the answer 
`30` is stored in `Z`. Finally `Z` is printed. 

## Functions in Monty

Functions are anonymous functions that can be assigned to variables and used as 
first-class citizens. They are a powerful feature of Monty that can be used to 
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

### Function with Multiple Arguments

You can also define functions with multiple arguments. For example:
```
\ab{ %a %b + . }
```
This function takes two arguments `a` and `b`, adds them together using the `+` operator, 
and then prints the result using `.`.

### Assigning Functions to Variables

In Monty, you can assign functions to variables just like any other value. 
Variables in Monty are limited to a single uppercase or lowercase letter. To 
assign a function to a variable, use the `:=` operator.

Let's see some examples:

```
\a{ %a . } A =
\ab{ %a %b + . } B =
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
\ab:c{%c := %a %b ; %c . }
```
This function takes two arguments, `a` and `b`, and a local variable, `c`. 
The body of the function first assigns the value of `a` plus `b` to `c`. 
Then, it prints the value of `c`.

Local variables in functions are only accessible within the scope of the function. 
This means that they cannot be accessed from outside the function.

Here are some more examples of local variables in functions:

A function with a single local variable
```
\a:c{ %c := 10 ; %c . }
```
The function takes a single argument, `a`, and a local variable, `c`. The body of 
the function first assigns the value of `10` to `c`. Then, it prints the value of `c`.

A function with two local variables
```
\ab:c{ %c := %a %b + ; %c . }
```
The function takes two arguments, `a` and `b`, and a local variable, `c`. The body 
of the function first assigns the value of `a` plus `b` to `c`. Then, it prints the 
value of `c`.

As you can see, local variables can be used to store temporary values in functions. 
This can make your code more concise and easier to read.

Here are some additional things to keep in mind about local variables in functions:

* Local variables must be declared before they are used.
* Local variables are only accessible within the scope of the function.
* Local variables can be overwritten within the scope of the function.


