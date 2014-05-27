#ifndef PIPE_H
#define PIPE_H

typedef struct pipe *Pipe;

char* getPipeName(Pipe p);

Pipe newPipe(char* name);

void deletePipe(Pipe p);

int* getDescriptors(Pipe p);

#endif
