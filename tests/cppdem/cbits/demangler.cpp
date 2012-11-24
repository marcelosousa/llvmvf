#include <cxxabi.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "../include/demangler.h"

    
char* demangler(char *argv){

size_t sz = 256;
char *buffer = (char *)malloc(sz);
char *symbol = argv;
char *safes = (char *)malloc(sizeof(argv)+1);

memcpy (safes,argv,strlen(argv)+1);

char *begin = strchr(symbol, '_');
char *demangled_name = NULL;
int status;

if(begin == NULL)
  return safes;

demangled_name = abi::__cxa_demangle(begin, buffer, &sz, &status);

if(demangled_name)  
  return demangled_name;
else 
  return safes;
}
