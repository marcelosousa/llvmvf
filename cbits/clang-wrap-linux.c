const char clang[]="/home/scratch/experiments/290413/llvmlinux/toolchain/clang/install/bin/clang-3.3";
const char llvmdis[]="/home/scratch/experiments/290413/llvmlinux/toolchain/clang/install/bin/llvm-dis ";

#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

#include <sys/wait.h>
#include <sys/types.h>

void run(const char *what, char *const argv[])
{
  pid_t childpid; /* variable to store the child's pid */
  int retval;     /* child process: user-provided return code */

  /* now create new process */
  childpid = fork();
    
  if(childpid>=0) /* fork succeeded */
  {
    if(childpid==0) /* fork() returns 0 to the child process */
    {
      execvp(what, argv);
      /* usually no return */
      printf("Waiting\n");
      fprintf(stderr, "execp %s failed\n", what);
      exit(1);
    }
    else /* fork() returns new pid to the parent process */
    {
      int status;     /* parent process: child's exit status */
      wait(&status); /* wait for child to exit, and store its status */
      int code=WEXITSTATUS(status);
      if(code!=0) exit(code);
    }
  }
  else /* fork returns -1 on failure */
  {
    perror("fork failed"); /* display error message */
    exit(1);
  }
}
 
int main(int argc, char * argv[])
{
  //printf("clang-wrapper\n");
  // First do original call.
  
  // on some systems, clang gets confused if it is not argument 0
  // (which normally contains the path to the executable being called).
  argv[0]=strdup(clang);

  run(clang, argv);
  
  // now do preprocessing call
  char **new_argv=malloc(sizeof(char *)*(argc+4));
  char **new_argvd=malloc(sizeof(char *)*(2));
  char *tmp;
  _Bool compile=0;
  _Bool assemble=0;
  
  unsigned i;

  for(i=0; i<argc; i++)
  {
    char *arg=argv[i];

    if(strcmp(arg+(strlen(arg))-2, ".c")==0){
      tmp=malloc(strlen(arg)+strlen(".bc"));
      strcpy(tmp, arg);
      strcat(tmp, ".bc"); // append .bc
      printf("tmp is set to %s\n",tmp);
    }
    else if(strcmp(arg, "-c")==0)
    {
      arg="-c";
      compile=1;
    }
    else if(strncmp(arg, "-", 1)!=0)
    {
      const char *ext=strrchr(arg, '.');
      if(ext!=NULL)
        if(strcmp(ext, ".S")==0 ||
           strcmp(ext, ".sx")==0 ||
           strcmp(ext, ".s")==0)
        {
          assemble=1;
        }
    }

    new_argv[i]=arg;
  }
  
  new_argv[argc+3]=NULL;
  
  if(compile && !assemble)
  {
    new_argv[argc]="-emit-llvm";
    new_argv[argc+1]="-o";
    new_argv[argc+2]=tmp;

    run(clang, new_argv);
    char *new_argvd=malloc(sizeof(llvmdis)+sizeof(tmp));
    strcpy(new_argvd,llvmdis);
    strcat(new_argvd,tmp);
    system(new_argvd);
  //  run(llvmdis, new_argvd);
  }
    
  return 0;
}
