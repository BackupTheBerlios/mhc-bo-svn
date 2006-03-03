#
# Makefile for Mono Haskell Compiler
#

subdir = /mhc-bo
top_builddir = ..
# Cuando Nesecitemos
#include $(top_builddir)/src/Makefile.global

uulib = -L /usr/local/lib/uulib/ -luu
#override CPPFLAGS := -I/usr/include/pgsql -O3 -msse -msse2 -msse3 $(CPPFLAGS)
#override LDLIBS := $(libpq) $(LDLIBS)


HS     = ghc --make -fglasgow-exts


AGS    = ags/AST.ag
EJEM   = test/P2.hs
README = README
MAIN   = mhc/Main.hs
PROGS  = mhc/AST.hs mhc/Datas.hs mhc/ParserAg.hs mhc/Parser.hs mhc/Scanner.hs

all: $(MAIN)

clean:
	rm -f $(PROGS) $(AGS) $(EJEM) $(README) $(MAIN)

