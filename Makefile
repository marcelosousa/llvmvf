CABAL-CONFIGURE-FLAGS := --user
CABAL-BUILD-FLAGS :=
VERSION	:= 0.0.1

BASEAG := src/Language/LLVMIR/Grammar/Base.ag
PRINTAG := src/Language/LLVMIR/Printer/Module.ag
TYPEAG := src/Language/LLVMIR/Grammar/Type.ag
ENCODEAG := src/Concurrent/Model/Encoder/Module.ag
PTHREADAG := src/Concurrent/Model/PThread.ag
VISUALCCFG := src/Concurrent/Model/Visualizer.ag
CFLOWAG := src/Concurrent/Model/Analysis/ControlFlow.ag

all : haskell

ag : base printer encoder pthread cflow ppccfg

base : $(BASEAG) $(TYPEAG)
	uuagc -Hd --datarecords --self -P src/Language/LLVMIR/Grammar src/Language/LLVMIR.ag

printer : base $(PRINTAG) 
	uuagc -Hcfws --self -P src/Language/LLVMIR/Grammar $(PRINTAG)

encoder : base $(ENCODEAG) 
	uuagc -Hcfws --self -P src/Language/LLVMIR/Grammar $(ENCODEAG)

pthread : base $(PTHREADAG) 
	uuagc -Hcfws --self -P src/Language/LLVMIR/Grammar $(PTHREADAG)

cflow : base $(CFLOWAG) 
	uuagc -Hcfws --self -P src/Language/LLVMIR/Grammar $(CFLOWAG)

ppccfg : base $(VISUALCCFG)
	uuagc -Hcfws --self -P src/Analysis -P src/Language/LLVMIR/Grammar -P src/Language/LLVMIR/Printer $(VISUALCCFG)

haskell : ag
	cabal install

#doc : README
#	asciidoc -a toc -a numbered README

dist:
	tar tfz imp-$(VERSION).tar.gz $(AG)

.PHONY : haskell dist doc

#	ghc -Wall -optl -w --make -o Parser Parser.hs

