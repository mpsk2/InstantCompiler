BNFC=bnfc
BNFC_URL_PREFIX=http://bnfc.digitalgrammars.com/download/
BNFC_URL_INFIX=BNFC-2.7.1-linux-x86_64
BNFC_URL_SUFFIX=.tar.gz
GRAMMAR_NAME=Instant
GRAMMAR_FILE=src/$(GRAMMAR_NAME).cf
GRAMMAR_DIR=src/$(GRAMMAR_NAME)

all: $(GRAMMAR_DIR)

$(GRAMMAR_DIR): bnfc $(GRAMMAR_FILE)
	./$(BNFC) $(GRAMMAR_FILE) -d
	mv $(GRAMMAR_NAME) $(GRAMMAR_DIR)
	rm $(GRAMMAR_DIR)/Test.hs
	sed -e'/^module Instant.Lex where/a import Data.Char' $(GRAMMAR_DIR)/Lex.x > $(GRAMMAR_DIR)/Lex2.x
	mv $(GRAMMAR_DIR)/Lex2.x $(GRAMMAR_DIR)/Lex.x
	

$(BNFC):
	wget $(BNFC_URL_PREFIX)$(BNFC_URL_INFIX)$(BNFC_URL_SUFFIX)
	tar -xf $(BNFC_URL_INFIX)$(BNFC_URL_SUFFIX)
	mv $(BNFC_URL_INFIX)/bin/bnfc $(BNFC)
	rm -rf $(BNFC_URL_INFIX)$(BNFC_URL_SUFFIX) $(BNFC_URL_INFIX)
	
clean:
	rm -rf $(BNFC) $(GRAMMAR_DIR)