# minimark — build with MicroHs.
#
#   make            native binary via mhs (needs mhs in PATH or MHS=...)
#   make c          portable minimark.c for the ppc box: compile there with
#                   cc -O2 minimark.c -lm -o minimark   (plus MicroHs runtime)
#
# On a 1GB G4, shrink the runtime heap if needed:  minimark +RTS -H8M -RTS ...

MHS ?= mhs
MHSFLAGS ?= -C -isrc

minimark: src/MiniMark/*.hs
	$(MHS) $(MHSFLAGS) MiniMark.Main -ominimark

c: src/MiniMark/*.hs
	$(MHS) $(MHSFLAGS) MiniMark.Main -ominimark.c

clean:
	rm -f minimark minimark.c .mhscache

.PHONY: c clean
