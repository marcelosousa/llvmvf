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

//const int arg_default = 12;

class element;
element * ELEMENT;

class element : public sc_module
{
public:
  sc_event local;
  sc_event global;

  int x;
  int num;
  SC_HAS_PROCESS(element);
  element(sc_module_name name, int a, int n) :
    sc_module(name),x(a),num(n)
  {
    SC_THREAD(P1);
    SC_THREAD(P2);
  }

  void P1();
  void P2();
};

void element::P1() {
  wait(1,SC_NS);
  if (x<=num && x > 0 && num > 0) {
    local.notify();
    wait(local);
  }
  cout << "p1" << endl;
  local.notify();  
  wait(global);
 
  cout <<"Element " <<num <<" OK.\n";
}

void element::P2() {
  if (x<=num) {
    cout << "p2" << endl;
    wait(local);
    x++;
    cout << "p2 " << x << endl;
    local.notify();
  }
  if(num > 1){
    wait(local);
//    global.notify();
  }
}


/*
class top : public sc_module
{
public:
  vector<sc_event *> events;
  vector<element *> elements;

  top(sc_module_name name,int num) :
    sc_module(name) {
    char s_name[16];

    for (int i=0;i<num;i++) {
      sprintf(s_name,"element_%d",i+1);
      elements.push_back(new element(s_name,i));
      sprintf(s_name,"event_%d",i+1);
      events.push_back(new sc_event(s_name));
    }
  }
};
*/

int sc_main(int argc , char *argv[])
{
  int num = 0;
  int a = 0;

  if (argc > 1){
    num = atoi(argv[1]);
    a = atoi(argv[2]);    
  }
/*
  if (num < 1)
    num = arg_default;
  if (num > 100)
    num = 100;
*/
  ELEMENT = new element("element",a,num);
  sc_start();
  return(0);
}

