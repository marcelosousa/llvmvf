CABAL-CONFIGURE-FLAGS := --user
CABAL-BUILD-FLAGS :=
VERSION	:= 0.0.1

BASEAG := src/Language/LLVMIR/Grammar/Base.ag
PRINTAG := src/Language/LLVMIR/Printer/Module.ag
STYPEAG := src/Language/LLVMIR/Type/Standard.ag
CONVERTAG := src/Language/LLVMIR/Converter/Module.ag
PTHREADAG := src/Concurrent/Model/Domain/PThread.ag
VISUALCCFG := src/Concurrent/Model/Visualizer.ag
AUXAG := src/Concurrent/Model/Analysis/Util.ag
DFLOWAG := src/Concurrent/Model/Analysis/DataFlow.ag
ESENCODEAG := src/Concurrent/Model/ESEncoder/Model.ag
ENCODEAG := src/Concurrent/Model/Encoder/Model.ag
TENCODEAG := src/Concurrent/Model/Encoder/Threads.ag
SYSTEMCAG := src/Concurrent/Model/Analysis/SystemC.ag
ARCHSYSCAG := src/Concurrent/Model/Analysis/SystemC/Architecture.ag
 
AGFLAGS := -P src/Language/LLVMIR/Grammar -P src/Language/LLVMIR/Type

all : haskell

ag : base printer pthread ppccfg encoder converter auxiliar

base : $(BASEAG) $(TYPEAG)
	uuagc -Hd --datarecords --self $(AGFLAGS) src/Language/LLVMIR.ag

printer : base $(PRINTAG) 
	uuagc -Hcfws --self $(AGFLAGS) $(PRINTAG)

converter : base $(CONVERTAG) 
	uuagc -Hcfws --self $(AGFLAGS) $(CONVERTAG)

esencoder : base 
	uuagc -Hcfws --self $(AGFLAGS) -P src/Concurrent/Model/ESEncoder $(ESENCODEAG)

encoder : base 
	uuagc -Hcfws --self $(AGFLAGS) -P src/Concurrent/Model/Encoder $(ENCODEAG)
	uuagc -Hcfws --self $(AGFLAGS) -P src/Concurrent/Model/Encoder $(TENCODEAG)

pthread : base $(PTHREADAG) 
	uuagc -Hcfws --self $(AGFLAGS) $(PTHREADAG)

systemc : base $(SYSTEMCAG) $(ARCHSYSCAG) 
	uuagc -Hcfws --self $(AGFLAGS) -P src/Concurrent/Model/Analysis/SystemC $(SYSTEMCAG)

auxiliar : base $(AUXAG) 
	uuagc -Hcfws --self $(AGFLAGS) $(AUXAG)

dflow : base $(DFLOWAG) 
	uuagc -Hcfws --self $(AGFLAGS) $(DFLOWAG)

ppccfg : base $(VISUALCCFG)
	uuagc -Hcfws --self -P src/Analysis $(AGFLAGS) -P src/Language/LLVMIR/Printer $(VISUALCCFG)

haskell : ag
	cabal install

#doc : README
#	asciidoc -a toc -a numbered README

dist:
	tar tfz imp-$(VERSION).tar.gz $(AG)

.PHONY : haskell dist doc

#	ghc -Wall -optl -w --make -o Parser Parser.hs

