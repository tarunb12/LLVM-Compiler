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

Each file must have a main method as the entry point to the program (which LLVM requires). The main method should look as follows:

```c
int main() {
   ...
}
```

By default, main will return the 32-bit integer 0, so the return type is optional.

In general, methods will be structured as follows:
```c
type fname(arg1, arg2, ..., argn) {
   ...
   return n
}
```
where type can be one of "int", "float", "bool", "char", or "string", and 'n' is of the same type. Booleans are
encoded as 1-bit integers, characters as 8-bit integers, and Integers as 32-bit integers. Floats are 32-bit floating
point types, and strings are 8-bit integer pointers. When performing binary operations on these types (excluding string), both expressions being operated on should have the same type, or an error will be thrown, even though many
of these types are stored as integers. Floats cannot perform

To print values, the printf function is built into this language, and is very similar to that of c. For example,  
`printf("%d + %d = %d", 1, 2, 1 + 2)` will produce `1 + 2 = 3` in the output file. Expressions are NOT automatically printed when they are evaluated, so expressions will need to go through the printf call to be shown in the output file.