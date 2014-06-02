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

char* getPipeName(Pipe p) {
  return p -> name;
}

int getChildPid(Pipe p) {
  return p -> child_pid;
}

Pipe newPipe(char* name) {
  Pipe new = (Pipe)malloc( sizeof(struct pipe) );

  new -> name = str_dup(name);
  new -> fd = (int*)malloc(sizeof(int) * 2);
  new -> child_pid = -1;
  pipe(new -> fd);

  return new;
}

void setDescriptors(int* fd, Pipe p) {
  (p -> fd)[0] = fd[0];
  (p -> fd)[1] = fd[1];
}

void setChildPid(int pid, Pipe p) {
  p -> child_pid = pid;
}

void deletePipe(Pipe p) {
  free(p -> name);
  free(p -> fd);
  free(p);
}

void getDescriptors(Pipe p, int** ret) {
  (*ret)[0] = (p -> fd)[0];
  (*ret)[1] = (p -> fd)[1];
}

void closeChild(Pipe p) {
  int fd = (p -> fd)[1];
  write(fd, "0\0", 2);
  close(fd);
}

