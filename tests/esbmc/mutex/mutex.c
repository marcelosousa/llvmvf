#include <pthread.h>
#include <stdio.h>
#include <assert.h>

pthread_mutex_t m;
int nondet_int();
int x=2;

void *Tx(void *arg) 
{
  pthread_mutex_lock(&m);
  x=3;
  pthread_mutex_unlock(&m);
}

void *Ty(void *arg) 
{
  pthread_mutex_lock(&m);
  assert(x==2);
  pthread_mutex_unlock(&m);
}

int main() 
{
  pthread_t t1, t2;

  pthread_mutex_init(&m, 0);

  pthread_create(&t1, 0, Tx, 0);
  pthread_create(&t2, 0, Ty, 0);

  return 0;
}
