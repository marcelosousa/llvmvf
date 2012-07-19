/*****************************************************************************

  (C) Copyright 2004-2007 by CNRS and STMicroelectronics.
  All Rights reserved.

  The contents of this file are subject to the restrictions and limitations
  set forth in the SystemC Open Source License Version 2.4 (the "License");
  You may not use this file except in compliance with such restrictions and
  limitations. You may obtain instructions on how to receive a copy of the
  License at http://www.systemc.org/. Software distributed by Contributors
  under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
  ANY KIND, either express or implied. See the License for the specific
  language governing rights and limitations under the License.

 *****************************************************************************/

#include <systemc.h>

class top : public sc_module
{
  public:
  int x,y,z;
  SC_HAS_PROCESS(top);
  top(sc_module_name name, int a, int b, int c) :
    sc_module(name) {
    x=a; y=b; z=c;
    SC_THREAD(P);
    SC_THREAD(Q);
    SC_THREAD(R);
  }

  void P() {
    cout <<"p";
    if(x==0 && (y > 0 || z > 0)){
      wait(10,SC_NS);
      x=1;
    }

    cout <<"p";
    wait(20,SC_NS);
    x=0; 
    cout <<" xyz: " <<x <<y <<z <<endl;
  }
  void Q() {
    cout <<"q";
    if(y==0 && z > 0){
      wait(10,SC_NS);
      y=1;
    }

    z=2;
    x=2;
  }
  void R() {
    cout <<"r";
    if(z==0){
      wait(10,SC_NS);
      z=1;
    }

    z=3;
    y=3;
  }
};

int sc_main(int argc , char *argv[])
{
  int x = atoi(argv[1]);
  int y = atoi(argv[2]);
  int z = atoi(argv[3]);

  top TOP("TOP",x,y,z);
  sc_start();
  return 0;
}
