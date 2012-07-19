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

#include <string>
#include "systemc.h"

class top : public sc_module
{
  public:
//  rvs_probe<unsigned> x;
//  rvs_probe<unsigned> y;
  int x, y;
  sc_event e;
  SC_HAS_PROCESS(top);
  top(sc_module_name name, int a, int b) :
    sc_module(name),x(a),y(b) {
    SC_THREAD(P);
    SC_THREAD(Q);
    SC_THREAD(R);
    SC_THREAD(S);
  }

  void P() {
    if(x){
      y=0;
      cout <<"p " <<x << " " << y << endl;
      wait(12,SC_NS);
    }
    cout <<"p " <<x << " " << y << endl;
  }

  void Q() {
    wait(e);
    x = 19;
    cout <<"q " <<x << " " << y << endl;
  }

  void R() { 
    wait(12,SC_NS);
    if (!y){
      e.notify();
      x=10;
      cout <<"r " <<x << " " << y << endl;
    }
    cout <<"r " <<x << " " << y << endl;
  }

  void S() {
    wait(12,SC_NS);
    y = 1;
    cout <<"s " <<x << " " << y << endl;
  }
};

int sc_main(int argc , char *argv[])
{
  int x = atoi(argv[1]);
  int y = atoi(argv[2]);

  top TOP("TOP", x,y);
  sc_start();
  return 0;
}
