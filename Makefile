
H_OPTS=-W -O2 -threaded -L/usr/lib
ifdef PROF
H_OPTS+=-prof -auto-all -L/usr/lib
endif

LIB_FILES=$(shell find WJR -name '*.hs' \! -name '.*' )

BINARIES=main runner

.PHONY: all

all: $(BINARIES)

% : %.hs $(LIB_FILES)
	ghc $(H_OPTS) --make $<

clean:
	rm -f *.o *.hi $(BINARIES)
	find . \( -name '*.o' -o -name '*.hi' \) -exec rm {} \;

