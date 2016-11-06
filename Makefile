BNFC_URL_PREFIX=http://bnfc.digitalgrammars.com/download/
BNFC_URL_INFIX=BNFC-2.7.1-linux-x86_64
BNFC_URL_SUFFIX=.tar.gz

all: bnfc

bnfc:
	wget $(BNFC_URL_PREFIX)$(BNFC_URL_INFIX)$(BNFC_URL_SUFFIX)
	tar -xf $(BNFC_URL_INFIX)$(BNFC_URL_SUFFIX)
	mv $(BNFC_URL_INFIX)/bin/bnfc .
	rm -rf $(BNFC_URL_INFIX)$(BNFC_URL_SUFFIX) $(BNFC_URL_INFIX)
	
clean:
	rm bnfc