IN_FILES=$(wildcard tests/*.in)
LLVM_FILES=$(IN_FILES:.in=.ll)
OUT_FILES=$(IN_FILES:.in=.out)

default: main.byte

clean:
	rm -f main.byte
	rm -rf _build/
	rm -rf tests/*.ll


main.byte:
	ocamlbuild -use-ocamlfind -use-menhir -pkgs llvm src/main.byte 

tests/%.ll: tests/%.in main.byte
	./main.byte $< > $@

tests: main.byte $(LLVM_FILES)
	@echo "Done testing. Results in tests dir."

retest: clean tests
