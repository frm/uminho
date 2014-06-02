#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include "pipe.h"
#include <strutil.h>
#include <stdio.h>

struct pipe {
  char* name;
  int* fd;
  int child_pid;
};

/**Retrieves the name of a pipe*/
char* getPipeName(Pipe p) {
  return p -> name;
}
/**Retrieves the PID of the child process reading the pipe in "Pipe p"*/
int getChildPid(Pipe p) {
  return p -> child_pid;
}

/**Creates a new "Pipe"*/
Pipe newPipe(char* name) {
  Pipe new = (Pipe)malloc( sizeof(struct pipe) );

  new -> name = str_dup(name);
  new -> fd = (int*)malloc(sizeof(int) * 2);
  new -> child_pid = -1;
  pipe(new -> fd);

  return new;
}


/**Sets the file descriptors in a "Pipe"*/
void setDescriptors(int* fd, Pipe p) {
  (p -> fd)[0] = fd[0];
  (p -> fd)[1] = fd[1];
}

/**Sets the child PID in "Pipe p"*/
void setChildPid(int pid, Pipe p) {
  p -> child_pid = pid;
}

/**Deletes the "Pipe"*/
void deletePipe(Pipe p) {
  free(p -> name);
  free(p -> fd);
  free(p);
}
/**Retrieves the file descriptors in "Pipe p", writes them in the "ret" array*/
void getDescriptors(Pipe p, int** ret) {
  (*ret)[0] = (p -> fd)[0];
  (*ret)[1] = (p -> fd)[1];
}

/**Closes "Pipe p", writes 0 to it, telling the child the pipe is out of use*/
void closeChild(Pipe p) {
  int fd = (p -> fd)[1];
  write(fd, "0\0", 2);
  close(fd);
}

