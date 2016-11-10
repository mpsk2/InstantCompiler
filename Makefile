define colorecho
	@tput setaf 2
	@echo $1
	@tput sgr0
endef

BNFC=bnfc
CABAL=cabal
GRAMMAR_NAME=Instant
GRAMMAR_FILE=src/$(GRAMMAR_NAME).cf
GRAMMAR_DIR=src/$(GRAMMAR_NAME)
INSC_JVM=insc_jvm
INSC_LLVM=insc_llvm
JASMIN=tools/jasmin.jar


all:
	$(call colorecho, "Compiling")
	$(CABAL) configure
	$(CABAL) build
	cp dist/build/$(INSC_JVM)/$(INSC_JVM) .
	cp dist/build/$(INSC_LLVM)/$(INSC_LLVM) .
	$(call colorecho, "Compiled")
	
clean:
	$(CABAL) clean
	rm -rf $(INSC_JVM) $(INSC_LLVM)
