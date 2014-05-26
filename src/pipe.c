#include <string.h>
#include <stdlib.h>
#include "pipe.h"

struct pipe {
  char* name;
  int fd[2];
};

char* getPipeName(Pipe p) {
  return p -> name;
}

Pipe newPipe(char* name, int fdesc[]) {
  Pipe new = (Pipe)malloc( sizeof(struct pipe) );

  new -> name = strdup(name);
  new -> fd = (int*)malloc(sizeof(int) * 2);

  for (int i = 0; i < 2; i++)
    (new -> fd)[i] = fdesc[i];

  return new;
}

void deletePipe(Pipe p) {
  free(p -> name);
  free(p -> fd);
  free(p);
}
