CABAL-CONFIGURE-FLAGS := --user
CABAL-BUILD-FLAGS :=
VERSION	:= 0.0.1

BASEAG := src/Language/LLVMIR/Grammar/Base.ag
PRINTAG := src/Language/LLVMIR/Printer/Module.ag
TYPEAG := src/Language/LLVMIR/Grammar/Type.ag
ENCODEAG := src/Language/LLVMIR/Encoder/Module.ag

all : haskell

base : $(BASEAG) $(TYPEAG)
	uuagc -Hd --self -P src/Language/LLVMIR/Grammar src/Language/LLVMIR.ag

printer : base $(PRINTAG) 
	uuagc -Hcfws --self -P src/Language/LLVMIR/Grammar $(PRINTAG)

encoder : base $(ENCODEAG) 
	uuagc -Hcfws --self -P src/Language/LLVMIR/Grammar $(ENCODEAG)

haskell : base printer encoder
	cabal install

#doc : README
#	asciidoc -a toc -a numbered README

dist:
	tar tfz imp-$(VERSION).tar.gz $(AG)

.PHONY : haskell dist doc

#	ghc -Wall -optl -w --make -o Parser Parser.hs

