
H_OPTS=-W -O2 -threaded -L/usr/lib -fwarn-missing-signatures
ifdef PROF
H_OPTS+=-prof -auto-all -L/usr/lib
endif

ifdef PRODUCTION
H_OPTS+=-DPRODUCTION=1
endif

LIB_FILES=$(shell find WJR -name '*.hs' \! -name '.*' )

BINARIES=prokka-web prokka-runner

.PHONY: all

all: $(BINARIES)

% : %.hs $(LIB_FILES)
	ghc $(H_OPTS) --make $<

clean:
	rm -f *.o *.hi $(BINARIES)
	find WJR \( -name '*.o' -o -name '*.hi' \) -exec rm {} \;

