/*****************************************************************************

  (C) Copyright 2012 - Marcelo Sousa and Alper Sen 
  All Rights reserved.

 *****************************************************************************/

#include <systemc.h>
#include <stdlib.h>

SC_MODULE(M1)
{
  SC_HAS_PROCESS(M1);

  M1(sc_module_name name){
    SC_THREAD(M1T1);
    SC_THREAD(M1T2);
//    SC_THREAD(T3);
//    SC_THREAD(T4);
  }

  void M1T1() {
    for(int i=0;i<100;i++){
      printf("---------------\nM1T1 %d\n",i);
      wait(5,SC_NS);
    }
  }

  void M1T2(){
    for(int j=0;j<100;j++){
      printf("M1T2 %d\n",j);
      wait(5,SC_NS);
    }
  }
/*
  void T3(){
    for(int k=0;k<1000;k++){
      printf("T3 %d\n",k);
      wait(5,SC_NS);
    }
  }

  void T4(){
    for(int l=0;l<1000;l++){
      printf("T4 %d\n",l);
      wait(5,SC_NS);
    }
  }
*/
};

SC_MODULE(M2)
{

  SC_HAS_PROCESS(M2);

  M2(sc_module_name name){
    SC_THREAD(M2T1);
    SC_THREAD(M2T2);
  }

  void M2T1() {
    for(int i=0;i<100;i++){
      printf("M2T1 %d\n",i);
      wait(5,SC_NS);
    }
  }

  void M2T2(){
    for(int j=0;j<100;j++){
      printf("M2T2 %d\n",j);
      wait(5,SC_NS);
    }
  }

};
int sc_main(int argc , char *argv[])
{

  M1 M1("M1");
  M2 M2("M2");
  sc_start();

  return 0;
}
