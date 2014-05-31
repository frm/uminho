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

  return new;
}

void deletePipe(Pipe p) {
  free(p -> name);
  free(p -> fd);
  free(p);
}

int* getDescriptors(Pipe p) {
  return p -> fd;
}

