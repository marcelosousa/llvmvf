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
#include <vector>
using namespace std;

// #define max 4

const int arg_default = 12;

const int size = 128;
int table[size];

int hash(int w) {
  return (w*7)%size;
}

//class top;
//top * TOP;

class element : public sc_module
{
public:
  int m;
  int max;
  int msg;
  int h;
  sc_event msg_event;

  SC_HAS_PROCESS(element);
  element(sc_module_name name, int mm, int mmax) :
    sc_module(name), m(mm), max(mmax), msg(0), h(1)
  {
    SC_THREAD(T);
    SC_THREAD(getmsg);
  }

  void T() {
    if(m>0 && max > 0)
      wait(20,SC_NS);
    if (m<max+1) {    
      msg_event.notify();
      wait(msg_event);
      h = hash(msg);
      while (table[h] != 0) {
        h = (h+1) % size;
      }
      table[h] = msg;
    }
  }

  void getmsg() {
    if (m<max) {
      wait(msg_event);
      msg = (++m) * 11 + 5;
      msg_event.notify();
    }
  }
};

/*
class top : public sc_module
{
public:

  vector<element *> elements;

  top(sc_module_name name,int num) :
    sc_module(name) {
    char s_name[16];

    for (int i=0;i<num;i++) {
      sprintf(s_name,"element_%d",i+1);
      elements.push_back(new element(s_name,i));
    }
  }
};
*/

void dump_table() {
  cout <<"DUMP ";
  for (int n=0;n<size;n++)
    cout <<table[n] <<" "; //patch
  cout <<endl;
}

int sc_main(int argc , char *argv[])
{
  int num = arg_default;
  int max = 0;

  if (argc > 2){
    num = atoi(argv[1]);
    max = atoi(argv[2]);
  }

  if (num < 1)
    num = arg_default;
  if (num > 100)
    num = 100;

//  TOP = new top("TOP",num);
  element *ELEMENT = new element("element",num,max);
  sc_start();
  cout << "END ";
  dump_table();
  return(0);
}
