# Compile options (OCaml)
OC=ocamlbuild
OFLAGS=-use-menhir -use-ocamlfind -pkgs llvm

# Compile options (LLVM)
CC=clang
CFLAGS=-Wno-override-module

# Main file that results from ocaml compile
MAIN_FILE=main.byte

# Base folders for each of the test outputs
BUILD_BASE=_build/
SRC_BASE=src/
IN_BASE=tests/
LLVM_BASE=llvm/
EXE_BASE=executables/
OUT_BASE=results/

# Extensions for each of the files which the tests will produce
IN_EXT=.in
LLVM_EXT=.ll
EXE_EXT=.exe
OUT_EXT=.out

# Full directories in tests
TESTS_DIR=$(IN_BASE)
MAIN_DIR=$(addprefix $(SRC_BASE), $(MAIN_FILE))
LLVM_DIR=$(addsuffix $(LLVM_BASE), $(TESTS_DIR))
EXE_DIR=$(addsuffix $(EXE_BASE), $(TESTS_DIR))
OUT_DIR=$(addsuffix $(OUT_BASE), $(TESTS_DIR))

# Full path for each test input/output file
FILES=$(notdir $(basename $(wildcard $(TESTS_DIR)*$(IN_EXT))))
IN_FILES=$(addprefix $(TESTS_DIR), $(addsuffix $(IN_EXT), $(FILES)))
LLVM_FILES=$(addprefix $(LLVM_DIR), $(addsuffix $(LLVM_EXT), $(FILES)))
EXE_FILES=$(addprefix $(EXE_DIR), $(addsuffix $(EXE_EXT), $(FILES)))
OUT_FILES=$(addprefix $(OUT_DIR), $(addsuffix $(OUT_EXT), $(FILES)))

# Wildcard file names for each input/output
IN_WILD=$(TESTS_DIR)%$(IN_EXT)
LLVM_WILD=$(LLVM_DIR)%$(LLVM_EXT)
EXE_WILD=$(EXE_DIR)%$(EXE_EXT)
OUT_WILD=$(OUT_DIR)%$(OUT_EXT)

# All files for each output
LLVM_ALL=$(LLVM_DIR)*$(LLVM_EXT)
EXE_ALL=$(EXE_DIR)*$(EXE_EXT)
OUT_ALL=$(OUT_DIR)*$(OUT_EXT)


default: $(MAIN_FILE)

clean:
	rm -f $(MAIN_FILE)
	rm -rf $(BUILD_BASE)
	rm -rf $(LLVM_ALL)
	rm -rf $(EXE_ALL)
	rm -rf $(OUT_ALL)

$(MAIN_FILE):
	$(OC) $(OFLAGS) $(MAIN_DIR)

$(LLVM_WILD): $(IN_WILD) $(MAIN_FILE)
	./$(MAIN_FILE) $< $@
	
$(EXE_WILD): $(LLVM_WILD)
	$(CC) $(CFLAGS) $< -o $@

$(OUT_WILD): $(EXE_WILD)
	./$< > $@

tests: $(MAIN_FILE) $(LLVM_FILES) $(EXE_FILES) $(OUT_FILES)
	@echo "Done testing. Generated LLVM code in \"$(LLVM_ALL)\", Results in \"$(OUT_ALL)\"."

retest: clean tests