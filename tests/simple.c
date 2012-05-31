#include <assert.h>

void assertc(int expr){
  assert(expr);
}

int main(int argc, char **argv){
  int c = 1;
  assertc(c < 1);
  return 0;
}

