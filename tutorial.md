Introduction

What is Monty?

* History and purpose of Monty
* Key features and design principles
* Setting up the Monty environment
* Monty Basics

Monty syntax and conventions

* Numbers and basic arithmetic operations
* Variables and variable assignment
* Input and output functions
* Understanding Monty's stack

Control Flow in Monty

* Conditionals (? and ??)
* Loops (() and /br)
* Higher-order functions and lambdas (, {}, %, ^)

Data Structures

* Arrays ([] and #)
* Strings (', _, /si, /sl, /sb, /se, /ss)
* Basic operations on arrays and strings

Monty's Built-in Commands

* Logical operations (!, !=, ==, >, >=, <, <=, &, |, /x, /t, /f)
* Arithmetic operations (/, *, +, -, /ab, /re)
* Variables and variable operations (=, +=, ++, --, /ad, /vb, /vh, /vt, /vB, /vH, /vT, A..Z, a..z, :=, ;)
* Functions and lambda expressions (, {}, %, ^, /fs, /rc, /qt)

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

## Lambdas in Monty

Lambdas are anonymous functions that can be assigned to variables and used as 
first-class citizens. They are a powerful feature of Monty that can be used to 
simplify code and make it more concise.

### Basic Lambda Syntax

In Monty, lambdas are denoted by the `\` symbol followed by one or more arguments 
(single lowercase letters) and a `{` symbol to indicate the beginning of the 
lambda expression. The body of the lambda is written using Reverse Polish Notation (RPN), 
where `%` is used to reference the lambda's arguments.

A basic lambda with a single argument is represented as follows:
```
\a{ %a . }
```
This lambda takes a single argument `a` and prints its value using the `.` operator.

### Lambda with Multiple Arguments

You can also define lambdas with multiple arguments. For example:
```
\ab{ %a %b + . }
```
This lambda takes two arguments `a` and `b`, adds them together using the `+` operator, 
and then prints the result using `.`.

### Assigning Lambdas to Variables

In Monty, you can assign lambdas to variables just like any other value. 
Variables in Monty are limited to a single uppercase or lowercase letter. To 
assign a lambda to a variable, use the `:=` operator.

Let's see some examples:
```
A := \a{ %a . } ;
B := \ab{ %a %b + . } ;
```
Here, we assigned the lambdas to variables `A` and `B` respectively. 

`A` represents the lambda that takes a single argument and prints it, while `B` 
represents the lambda that takes two arguments, adds them, and prints the result.

### Using Lambdas

Once you've assigned lambdas to variables, you can use them in your Monty code. 
To execute a lambda and pass arguments to it, use the `^` operator. The lambda's 
body will be executed, and the result, if any, will be printed.

Example:
```
10 A^       // prints 10
3 7 B^      // prints 10, the sum of 3 and 7
```

In the first line, we execute the lambda stored in variable `A` with the argument `10`, 
which prints `10`. In the second line, we execute the lambda stored in variable `B` with 
arguments `3` and `7`, which results in `10` being printed (the sum of the two arguments).

### Higher-Order Functions

Monty allows you to create higher-order functions that take other functions (lambdas) 
as arguments or return them as results. This enables powerful and flexible programming.

For example, consider the following lambda:
```
\ab{ %a %b^ }
```
This lambda takes two arguments - a function a and an argument b. It then applies 
the function `a` to the argument `b` using the `^` operator.

### Lexical Scoping

In Monty, variables have lexical scoping, meaning they are only accessible within 
the scope they are defined in. If a variable is defined inside a lambda, it is 
accessible only within that lambda's body.

Example:
```
\x{ \y{ %x %y + . } . }
```
In this example, the inner lambda has access to the argument x of the outer lambda, 
but it cannot access any variables outside its scope.

Lambdas can also contain local variables. Local variables can be declared in 
lambdas by following the arguments with a colon and then one or more local variables. 
The syntax for declaring a local variable in a lambda is as follows:
```
\args:locals{ body }
```
For example, the following lambda contains a local variable c:
```
\ab:c{%c := %a %b ; %c . }
```
This lambda takes two arguments, `a` and `b`, and a local variable, `c`. 
The body of the lambda first assigns the value of `a` plus `b` to `c`. 
Then, it prints the value of `c`.

Local variables in lambdas are only accessible within the scope of the lambda. 
This means that they cannot be accessed from outside the lambda.

Here are some more examples of local variables in lambdas:

A lambda with a single local variable
```
\a:c{ %c := 10 ; %c . }
```
The lambda takes a single argument, `a`, and a local variable, `c`. The body of 
the lambda first assigns the value of `10` to `c`. Then, it prints the value of `c`.

A lambda with two local variables
```
\ab:c{ %c := %a %b + ; %c . }
```
The lambda takes two arguments, `a` and `b`, and a local variable, `c`. The body 
of the lambda first assigns the value of `a` plus `b` to `c`. Then, it prints the 
value of `c`.

As you can see, local variables can be used to store temporary values in lambdas. 
This can make your code more concise and easier to read.

Here are some additional things to keep in mind about local variables in lambdas:

* Local variables must be declared before they are used.
* Local variables are only accessible within the scope of the lambda.
* Local variables can be overwritten within the scope of the lambda.


