#include<pthread.h>
#include<assert.h>
#include<stdlib.h>
#include<stdio.h>
#include<time.h>

#define N 10

/*int nondet_uint(){
  unsigned int iseed = (unsigned int)time(NULL);
  srand (iseed);
  return rand();
}*/
int nondet_uint();

int a[N], i, j=1, x=2;

void *Tx(void *arg){
  if(x>2){
//    printf("Tx1\n");
    assert(i>=0 && i<N);
    a[i]=*((int *)arg);
  }
//  printf("Tx2\n");
  pthread_exit(NULL);
}

void *Ty(void *arg){
//  printf("Ty1\n");
  if(x>3)
    a[j]=*((int *)arg);
  else{
    x=3;
//    printf("Ty2\n");
  }
  pthread_exit(NULL);
}

int main(){
  pthread_t id1, id2;
  int arg1=10, arg2=20;
  i=nondet_uint()+1;
//  printf("%d\n",i);
  pthread_create(&id1, NULL, Tx, &arg1);
  pthread_create(&id2, NULL, Ty, &arg2);

  pthread_join(id1, NULL);
  pthread_join(id2, NULL);
//  pthread_exit(NULL);
}
