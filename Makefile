CABAL-CONFIGURE-FLAGS := --user
CABAL-BUILD-FLAGS :=
VERSION	:= 0.0.1

BASEAG := src/Language/LLVMIR/Grammar/Base.ag
PRINTAG := src/Language/LLVMIR/Printer/Module.ag
TYPEAG := src/Language/LLVMIR/Grammar/Type.ag
PTHREADAG := src/Concurrent/Model/PThread.ag
VISUALCCFG := src/Concurrent/Model/Visualizer.ag
CFLOWAG := src/Concurrent/Model/Analysis/ControlFlow.ag
DFLOWAG := src/Concurrent/Model/Analysis/DataFlow.ag
ESENCODEAG := src/Concurrent/Model/ESEncoder/Model.ag
ENCODEAG := src/Concurrent/Model/Encoder/Model.ag
TENCODEAG := src/Concurrent/Model/Encoder/Threads.ag
 
all : haskell

ag : base printer pthread cflow ppccfg  dflow encoder 

base : $(BASEAG) $(TYPEAG)
	uuagc -Hd --datarecords --self -P src/Language/LLVMIR/Grammar src/Language/LLVMIR.ag

printer : base $(PRINTAG) 
	uuagc -Hcfws --self -P src/Language/LLVMIR/Grammar $(PRINTAG)

esencoder : base 
	uuagc -Hcfws --self -P src/Language/LLVMIR/Grammar -P src/Concurrent/Model/ESEncoder $(ESENCODEAG)

encoder : base 
	uuagc -Hcfws --self -P src/Language/LLVMIR/Grammar -P src/Concurrent/Model/Encoder $(ENCODEAG)
	uuagc -Hcfws --self -P src/Language/LLVMIR/Grammar -P src/Concurrent/Model/Encoder $(TENCODEAG)

pthread : base $(PTHREADAG) 
	uuagc -Hcfws --self -P src/Language/LLVMIR/Grammar $(PTHREADAG)

cflow : base $(CFLOWAG) 
	uuagc -Hcfws --self -P src/Language/LLVMIR/Grammar $(CFLOWAG)

dflow : base $(DFLOWAG) 
	uuagc -Hcfws --self -P src/Language/LLVMIR/Grammar $(DFLOWAG)

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

