#include <cxxabi.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

    
int main(int argc, char* argv[]){

size_t sz = 256;
char * buffer = (char *)malloc(sz);
char * symbol = argv[1];

//printf("%s\n", symbol);
char * begin = strchr(symbol, '_');
char * demangled_name = NULL;
//if (begin) {
//  printf("Begin\n");
//  char * end = strchr(begin, ' ');
//  if (end) {
//    printf("Calling demangle\n");
//    *end = 0;
    int status;
    demangled_name = abi::__cxa_demangle(begin, buffer, &sz, &status);
//  }
//}
         
if (demangled_name != NULL) 
  printf("%s\n", demangled_name);
  
  
  return 0;
}
