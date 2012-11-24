#include <cxxabi.h>
#include <stdlib.h>
#include <string.h>

#include "demangler.h"
    
char* demangler(char *argv){

size_t sz = 256;
char *buffer = (char *)malloc(sz);
char *symbol = argv;

char *begin = strchr(symbol, '_');
char *demangled_name = NULL;
int status;
demangled_name = abi::__cxa_demangle(begin, buffer, &sz, &status);
  
return demangled_name;
}
