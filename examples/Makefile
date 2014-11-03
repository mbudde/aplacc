
APLACC = ../dist/build/aplacc/aplacc
APLACCFLAGS =

GHC = ghc
GHCFLAGS = -i../src

TEST_FILES = $(wildcard *.tail)

clean:
	rm -f $(basename $(TEST_FILES))

%-cuda: APLACCFLAGS += --cuda
%-cuda: GHCFLAGS += -threaded
%-cuda: %.hs
	$(GHC) $(GHCFLAGS) -o $@ $<
	rm $*.hi $*.o

%: %.hs
	$(GHC) $(GHCFLAGS) -o $@ $<
	rm $*.hi $*.o

%.hs: %.tail
	$(APLACC) $(APLACCFLAGS) $< > $@
