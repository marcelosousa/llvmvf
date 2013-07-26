#include<pthread.h>
#include<assert.h>

#define N 10

int nondet_uint();

int a[N], i, j=1, x=2;

void *Tx(void *arg){
  if(x>3){
    assert(i>=0 && i<N);
    a[i]=*((int *)arg);
  }
  pthread_exit(0);
}

void *Ty(void *arg){
  if(x>3)
    a[j]=*((int *)arg);
  else{
    x=3;
  }
  pthread_exit(0);
}

int main(){
  pthread_t id1, id2, id3;
  int arg1=10, arg2=20;

  i=nondet_uint();

  pthread_create(&id1, NULL, Tx, &arg1);
  pthread_create(&id2, NULL, Ty, &arg2);

  pthread_join(id1, NULL);
  pthread_join(id2, NULL);
}
