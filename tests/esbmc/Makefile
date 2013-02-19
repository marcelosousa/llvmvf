CXX=clang
CXXFLAGS=-c
CXXOFLAGS=-O3 -emit-llvm
SOURCES=$(wildcard *.c)
BYTECODE=$(SOURCES:.c=.bc)
OPT=/home/scratch/llvm-3.2.src/build/bin/opt
OPTFLAGS=-lowerinvoke -sink -strip-dead-prototypes -simplifycfg -simplify-libcalls -prune-eh -partial-inliner -tailcallelim -mem2reg -lowerswitch -indvars -loops -loop-simplify -loop-unroll -unroll-count=3 -instcombine -globalopt -constmerge -die -argpromotion -instnamer
OPTCODE=$(BYTECODE:.bc=o.bc)
DIS=/home/scratch/llvm-3.2.src/build/bin/llvm-dis
MOPTCODE=$(OPTCODE:o.bc=om.bc)

all: $(OPTCODE) 
	
$(BYTECODE): $(SOURCES)
	$(CXX) $(CXXFLAGS) $< $(CXXOFLAGS) -o $@

$(OPTCODE): $(BYTECODE) 
	$(OPT) $(OPTFLAGS) < $< > $@
	$(DIS) $@

original: $(SOURCES)
	$(CXX) $< -lsystemc -o prog

clean:
	rm *.ll *.bc *.log prog 
