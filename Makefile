CABAL-CONFIGURE-FLAGS := --user
CABAL-BUILD-FLAGS :=
VERSION	:= 0.0.1

BASEAG := src/Language/LLVMIR/Base.ag
PRINTAG := src/Language/LLVMIR/PPrinter.ag

all : haskell

llvmir : $(PRINTAG) $(BASEAG)
	uuagc -Hdcfws --self -P src/Language/LLVMIR src/Language/LLVMIR.ag

haskell : llvmir
	cabal install

#doc : README
#	asciidoc -a toc -a numbered README

dist:
	tar tfz imp-$(VERSION).tar.gz $(AG)

.PHONY : haskell dist doc

#	ghc -Wall -optl -w --make -o Parser Parser.hs

