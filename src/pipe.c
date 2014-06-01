#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include "pipe.h"

struct pipe {
  char* name;
  int* fd;
};

char* getPipeName(Pipe p) {
  return p -> name;
}

Pipe newPipe(char* name) {
  Pipe new = (Pipe)malloc( sizeof(struct pipe) );

  new -> name = strdup(name);
  new -> fd = (int*)malloc(sizeof(int) * 2);
  pipe(new -> fd);

  return new;
}

void setDescriptors(int* fd, Pipe p) {
  (p -> fd)[0] = fd[0];
  (p -> fd)[1] = fd[1];
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

