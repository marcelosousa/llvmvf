/*****************************************************************************

  (C) Copyright 2012 - Marcelo Sousa and Alper Sen 
  All Rights reserved.

 *****************************************************************************/

#include <systemc.h>
#include <stdlib.h>

SC_MODULE(M1)
{
  sc_event e;

  SC_HAS_PROCESS(M1);

  M1(sc_module_name name){
    SC_THREAD(T1);
    SC_THREAD(T2);
    SC_THREAD(T3);
    SC_THREAD(T4);
    SC_THREAD(T5);
  }

  void T1() {
    for(int i=0;i<10;i++){
      printf("------------------\nT1 %d\n",i);
      wait(5,SC_NS);
    }
  }

  void T2(){
    for(int j=0;j<10;j++){
      printf("T2 %d\n",j);
      wait(4,SC_NS);
    }
  }

  void T3(){
    for(int k=0;k<10;k++){
      printf("T3 %d\n",k);
      wait(5,SC_NS);
    }
  }

  void T4(){
    for(int l=0;l<10;l++){
      printf("T4 %d\n",l);
      wait(5,SC_NS);
    }
  }

  void T5(){
    for(int l=0;l<10;l++){
      printf("T5 %d\n",l);
      wait(5,SC_NS);
    }
  }
};

int sc_main(int argc , char *argv[])
{

  M1 M1("M1");
  sc_start();

  return 0;
}
