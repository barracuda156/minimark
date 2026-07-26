# minimark — build with MicroHs.
#
#   make            native binary: mhs generates minimark.c, cc compiles it
#                   against the vendored dist/runtime (needs mhs in PATH or
#                   MHS=..., a C compiler, zlib)
#   make c          regenerate portable minimark.c only
#   make dist       minimark.c -> dist/minimark.c (the lockstep copy that
#                   ships to boxes without mhs)
#
# The native build goes through the same C + runtime as the ppc dist
# build (one mhs run, one cc run): mhs's own link step puts -optl flags
# before the objects, which breaks -lz with GNU ld, and building against
# dist/runtime means the binary under test is the binary that ships.
#
# On a 1GB G4, shrink the runtime heap if needed:  minimark +RTS -H8M -RTS ...

MHS ?= mhs
MHSFLAGS ?= -C -isrc
CC ?= cc
CFLAGS ?= -O2
RT = dist/runtime

minimark: minimark.c cbits/mm_zlib.c cbits/mm_zlib.h
	$(CC) $(CFLAGS) -I$(RT) -I$(RT)/unix -Icbits \
	  $(RT)/main.c $(RT)/eval.c minimark.c cbits/mm_zlib.c \
	  -lm -lz -o minimark

minimark.c: src/MiniMark/*.hs
	$(MHS) $(MHSFLAGS) MiniMark.Main -ominimark.c

c: minimark.c

dist: minimark.c
	cp minimark.c dist/minimark.c

test: minimark
	tests/run.sh

clean:
	rm -f minimark minimark.c .mhscache

.PHONY: c dist clean test
