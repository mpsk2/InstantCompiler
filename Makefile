define colorecho
	@tput setaf 2
	@echo $1
	@tput sgr0
endef

BNFC=tools/bnfc
CABAL=cabal
GRAMMAR_NAME=Instant
GRAMMAR_FILE=src/$(GRAMMAR_NAME).cf
GRAMMAR_DIR=src/$(GRAMMAR_NAME)
INSC_JVM=insc_jvm
INSC_JVM_MAIN=ToJVM/Main.hs
JASMIN=tools/jasmin.jar


all:$(GRAMMAR_DIR) $(INSC_JVM_MAIN)
	$(call colorecho, "Compiling")
	$(CABAL) configure
	$(CABAL) build
	cp dist/build/$(INSC_JVM)/$(INSC_JVM) .
	$(call colorecho, "Compiled")

$(GRAMMAR_DIR): $(BNFC) $(GRAMMAR_FILE)
	$(call colorecho, "Grammar generating")
	./$(BNFC) $(GRAMMAR_FILE) -d
	mv $(GRAMMAR_NAME) $(GRAMMAR_DIR)
	sed -e'/^module Instant.Lex where/a import Data.Char' $(GRAMMAR_DIR)/Lex.x > $(GRAMMAR_DIR)/Lex2.x
	mv $(GRAMMAR_DIR)/Lex2.x $(GRAMMAR_DIR)/Lex.x
	$(call colorecho, "Grammar generated")
	
clean_dist:
	$(CABAL) clean

clean:
	$(CABAL) clean
	rm -rf $(GRAMMAR_DIR) $(INSC_JVM)
