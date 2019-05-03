# LLVM Compiler in OCaml

## Compiling and Running Tests
```console
$ make
$ make tests
```

In order to run the tests, you must have clang installed to compile the llvm into a binary executable.

Test inputs are read into the program, which produces its corresponding llvm file, compiled by clang into an executable, and running the executable produces the output file.

The input files can be found directly in the tests folder, and the produced llvm, executable, and out files will be in the llvm, executables, and results folder respectively within tests.

### Package Versions
| Package           | Version  |
|-------------------|:--------:|
| <sup>*</sup>LLVM  | 8.0.0    |
| menhir            | 20181113 |

<sup>*</sup>Both the LLVM compiler and opam package are version 8.0.0

## Language Features

This language is statically typed, ensuring all (return/function/variable) definitions are consistent with their type definitions throughout the program. If any errors are thrown, detailed error messages are printed in the output file rather than stopping the rest of the tests from executing.

### Syntax/Structure

Each file must have a main method as the entry point to the program (which LLVM requires). The main method should look as follows:

```c
int main() {
   ...
}
```

By default, main will return the 32-bit integer 0, so the return type is optional.

### Functions

In general, functions will be structured as follows:
```c
<type> fname(arg1, arg2, ..., argn) {
   ...
   return n
}
```
where type can be one of "int", "float", "bool", "char", or "string", and 'n' is of the same type. Recursive functions are also supported. Booleans are encoded as 1-bit integers, characters as 8-bit integers, and Integers as 32-bit integers. Floats are 32-bit floating point types, and strings are 8-bit integer pointers. When performing binary operations on these types (excluding string), both expressions being operated on should have the same type, or an error will be thrown, even though many of these types are stored as integers. Floats cannot perform certain operations, such as and, or, and xor.

Please note that for function calls to work, the function must be defined before the function call.

To print values, the printf function is built into this language, and is very similar to that of c. For example,  
`printf("%d + %d = %d", 1, 2, 1 + 2)` will produce `1 + 2 = 3` in the output file. Expressions are NOT automatically printed when they are evaluated, so expressions will need to go through the printf call to be shown in the output file.

### If Statement

If statements are structured as follows:

```c
   if (cond_1) {
      ...
   } else if (cond_2) {
      ...
   } ... else if (cond_n) {

   } else {
      ...
   }

   // Also valid:
   if (cond_1) ...
   else if (cond_2) ...
   ...
   else if (cond_n) ...
   else ...
```

Conditions in the if statement must evaluate to a bool type. Else blocks are NOT required for each of the if statements.

### For Statement

For statements are structured as follows:

```c
<int|float> i
for (i = 0; cond; i++) {
   ...
}
```

The condition (second part of the for loop) must evaluate to a bool type.

### While Statement

While statements are structured as follows:

```c
while (cond) {
   ...
}
```

The condition must evaluate to a bool type. While statements are implemented as for loops, but with the first and third parts of the for loop as null (leaving only the condition).

### Comments

The format for single line comments is
```c
// ...
```

and multiline comments is
```c
/*
   ...
*/
```

### Supported Operations

|                       | format             | int | float  | bool   | char   | string |
|-----------------------|:------------------:|:---:|:------:|:------:|:------:|:------:|
| negate                | -a                 | ✓   | ✓      |        | ✓      |        |
| increment             | a++                | ✓   |        |        | ✓      |        |
| decrement             | a--                | ✓   |        |        | ✓      |        |
| plus                  | a + b              | ✓   | ✓      |        | ✓      |        |
| minus                 | a - b              | ✓   | ✓      |        | ✓      |        |
| multiply              | a * b              | ✓   | ✓      |        | ✓      |        |
| divide                | a / b              | ✓   | ✓      |        | ✓      |        |
| modulo                | a % b              | ✓   | ✓      |        | ✓      |        |
| equality              | a == b             | ✓   | ✓      |        | ✓      |        |
| inequality            | a != b             | ✓   | ✓      |        | ✓      |        |
| less than             | a < b              | ✓   | ✓      |        | ✓      |        |
| less than / equal     | a <= b             | ✓   | ✓      |        | ✓      |        | 
| greater than          | a > b              | ✓   | ✓      |        | ✓      |        |
| greater than / equal  | a >= b             | ✓   | ✓      |        | ✓      |        |  
| not                   | !a                 |     |        | ✓      |        |        |
| and                   | a && b             |     |        | ✓      |        |        |
| or                    | a &vert;&vert; b   |     |        | ✓      |        |        |
| xor                   | a ^ b              |     |        | ✓      |        |        |