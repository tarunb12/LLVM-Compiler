# LLVM Compiler in OCaml

## Compiling and Running Tests
```console
$ make
$ make tests
```

In order to run the tests, you must have clang installed.

Test inputs are read into the program, which produces its corresponding llvm file, compiled by clang into an executable, and running the executable produces the output file.

The input files can be found directly in the tests folder, and the produced llvm, executable, and out files will be
in the llvm, executables, and results folder respectively within tests.

### Package Versions
| Package           | Version  |
|-------------------|:--------:|
| <sup>*</sup>LLVM  | 8.0.0    |
| menhir            | 20181113 |

<sup>*</sup>Both the LLVM compiler and opam package are version 8.0.0

## Language Features

### Syntax/Structure

If there are any errors within the tests, I have for the most part checked for those errors, and custom error messages
with more information on the error is output into the file, rather than producing a compiler error (so that the rest
of the test are still able to be compiled).

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
type fname(arg1, arg2, ..., argn) {
   ...
   return n
}
```
where type can be one of "int", "float", "bool", "char", or "string", and 'n' is of the same type. Booleans are
encoded as 1-bit integers, characters as 8-bit integers, and Integers as 32-bit integers. Floats are 32-bit floating
point types, and strings are 8-bit integer pointers. When performing binary operations on these types (excluding string), both expressions being operated on should have the same type, or an error will be thrown, even though many
of these types are stored as integers. Floats cannot perform certain operations, such as and, or, and xor.

To print values, the printf function is built into this language, and is very similar to that of c. For example,  
`printf("%d + %d = %d", 1, 2, 1 + 2)` will produce `1 + 2 = 3` in the output file. Expressions are NOT automatically printed when they are evaluated, so expressions will need to go through the printf call to be shown in the output file.

### Statements

#### If Statement

If statements are structured as follows:

```c
   if (cond_1) {
      ...
   }
   else if (cond_2) {
      ...
   }
   ...
   else if (cond_n) {

   }
   else {
      ...
   }

   // Also valid:
   if (cond_1) ...
   else if (cond_2) ...
   ...
   else if (cond_n) ...
   else ...
```

Conditions in the if statement must evaluate to a bool type. Else blocks are NOT required
for each of the if statements.

#### For Statement

For statements are structured as follows:

```c
<int|float> i
for (i = 0; cond; i++) {
   ...
}
```

The condition (second part of the for loop) must evaluate to a bool type.

#### While Statement

While statements are structured as follows:

```c
while (cond) {
   ...
}
```

The condition must evaluate to a bool type. While statements are implemented as for loops, but with the first and third
parts of the for loop as null (leaving only the condition).