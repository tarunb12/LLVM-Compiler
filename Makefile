CC=clang
CFLAGS=-Wno-override-module

IN_FILES=$(wildcard tests/*.in)
FILES=$(notdir $(IN_FILES))
LLVM_FILES= $(addprefix tests/llvm/, $(FILES:.in=.ll))
EXE_FILES=$(addprefix tests/executables/, $(FILES:.in=.exe))
OUT_FILES=$(addprefix tests/results/, $(FILES:.in=.out))

default: main.byte

clean:
	rm -f main.byte
	rm -rf _build/
	rm -rf tests/llvm/*.ll
	rm -rf tests/results/*.out
	rm -rf tests/executables/*.exe

main.byte:
	ocamlbuild -use-ocamlfind -use-menhir -pkgs llvm src/main.byte 

tests/llvm/%.ll: tests/%.in main.byte
	./main.byte $< $@
	
tests/executables/%.exe: tests/llvm/%.ll
	$(CC) $(CFLAGS) $< -o $@

tests/results/%.out: tests/executables/%.exe
	./$< > $@

tests: main.byte $(LLVM_FILES) $(EXE_FILES) $(OUT_FILES)
	@echo "Done testing. Results in 'tests/results' dir."

retest: clean tests
