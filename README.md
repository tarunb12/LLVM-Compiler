# LLVM Compiler in OCaml

## Compiling and Running Tests
```console
$ make
$ make tests
```
In order to run the tests, you must have clang installed.

Test inputs are read into the program, which produces its corresponding llvm file, compiled by clang into an executable, and running the executable produces the output file
(&lt;test&gt;.in  &rightarrow;  &lt;test&gt;.ll  &rightarrow;  &lt;test&gt;.exe  &rightarrow;  &lt;test&gt;.out).

### Package Versions
| Package           | Version  |
|-------------------|:--------:|
| <sup>*</sup>LLVM  | 8.0.0    |
| menhir            | 20181113 |

<sup>*</sup>Both the LLVM compiler and opam package are version 8.0.0