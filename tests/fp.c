#include<stdio.h>

int DoIt  (float a, char b, char c){ printf("DoIt\n");   return a+b+c; }

int main(){
  int (*pt2Function)(float, char, char) = NULL;// C
  pt2Function = DoIt;      // short form
  int result1 = pt2Function(12, 'a', 'b');          // C short way
  printf("%d\n",result1);
  return 0;
}
