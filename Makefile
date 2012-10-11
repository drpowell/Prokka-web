
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

install: all
	mkdir -p ~prokka/bin ~prokka/user-files/uploads ~prokka/user-files/status ~prokka/user-files/output
	cp main runner ~prokka/bin
	cp -r static ~prokka
	cp -r templates ~prokka
