/*****************************************************************************

  (C) Copyright 2012 - Marcelo Sousa and Alper Sen 
  All Rights reserved.

 *****************************************************************************/

#include <systemc.h>
#include <stdlib.h>

SC_MODULE(M1)
{
  sc_event e;
  bool cs1, cs2;

  SC_HAS_PROCESS(M1);

  M1(sc_module_name name, bool x, bool y){
    SC_THREAD(T1);
    SC_THREAD(T2);
    cs1 = x;
    cs2 = y;
  }

  void T1() {
    if(cs1){
      wait(e);
      cs2=false;
    }
    wait(10,SC_NS);
    cs2=true;
  }

  void T2(){
    if(cs2){
      cs1=false;
      e.notify();
    }
    wait(10,SC_NS);
    cs1=true;
  }
};

int sc_main(int argc , char *argv[])
{
  if(argc != 3){
    return -1;
  }

  int x = atoi(argv[1]);
  bool cs1=(x ? true : false);

  int y = atoi(argv[2]);
  bool cs2=(y ? true : false);

  M1 M1("M1", cs1, cs2);
  sc_start();

  return 0;
}
