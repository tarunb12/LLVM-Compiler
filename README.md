# LLVM Compiler in OCaml

## Compiling and Running Tests
```console
$ make
$ make tests
```

In order to run the tests, you must have clang installed.

Test inputs are read into the program, which produces its corresponding llvm file, compiled by clang into an executable, and running the executable produces the output file.

```ocaml
   <test_name>.in
|> <test_name>.ll
|> <test_name>.exe
|> <test_name>.out
```

Note: llvm, exe, and out files produced from tests will be in tests/llvm, tests/executables, and tests/results, respectively.

### Package Versions
| Package           | Version  |
|-------------------|:--------:|
| <sup>*</sup>LLVM  | 8.0.0    |
| menhir            | 20181113 |

<sup>*</sup>Both the LLVM compiler and opam package are version 8.0.0