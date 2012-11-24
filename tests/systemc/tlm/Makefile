CXX=/home/marcelosousa/Tools/clang/bin/clang++
CXXFLAGS=-c
CXXOFLAGS=-O3 -emit-llvm -I/home/marcelosousa/Tools/TLM-2008-06-09/include/tlm/
SOURCES=$(wildcard *.cpp)
BYTECODE=$(SOURCES:.cpp=.bc)
OPT=/home/marcelosousa/Tools/llvm/build/bin/opt
OPTFLAGS=-lowerinvoke -sink -strip-dead-prototypes -simplifycfg -simplify-libcalls -prune-eh -partial-inliner -tailcallelim -mem2reg -lowerswitch -indvars -loops -loop-simplify -loop-unroll -unroll-count=3 -instcombine -globalopt -constmerge -die -argpromotion -instnamer
OPTCODE=$(BYTECODE:.bc=o.bc)
DIS=/home/marcelosousa/Tools/llvm/build/bin/llvm-dis
MOPTCODE=$(OPTCODE:o.bc=om.bc)

all: $(OPTCODE) original
	
$(BYTECODE): $(SOURCES)
	$(CXX) $(CXXFLAGS) $< $(CXXOFLAGS) -o $@

$(OPTCODE): $(BYTECODE) 
	$(OPT) $(OPTFLAGS) < $< > $@
	$(DIS) $@
	$(OPT) -load ~/Tools/llvm/build/lib/LLVMMutation.so -mutation < $@ > metamutant.bc

original: $(SOURCES)
	$(CXX) $< -lsystemc -I/home/marcelosousa/Tools/TLM-2008-06-09/include/tlm/ -o prog

clean:
	rm *.ll *.bc *.log prog 
