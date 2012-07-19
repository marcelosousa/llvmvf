#include <systemc.h>
struct sc_verif
{
	static void ASSERT(bool cond) {
		//abort();
		;
	}
};

using namespace sc_core;

class Source : public sc_module {
public:
	sc_out<bool> out;
	void compute() {
		out.write(true); // <----
	}
	SC_CTOR(Source) {
		SC_THREAD(compute);
	}
};

class Target : public sc_module {
public:
	sc_in<bool> in;
	void compute() {
		int i = 0;
		while (in.read() == true && i < 10) {
			wait(0, SC_NS);
			i++;
		}
		if (i != 10)
			sc_verif::ASSERT(false);
	}
	SC_CTOR(Target) {
		SC_THREAD(compute);
	}
};

int sc_main (int argc , char *argv[]) 
{
 	sc_signal<bool> s1;

 	Source source("source");
 	Target target("target");
       
 	source.out(s1); target.in(s1);

	//Run the Simulation for "200 nanosecnds"
	sc_start(200, SC_NS);

	return 0;
}
