#ifndef PIPE_H
#define PIPE_H

typedef struct pipe *Pipe;

char* getPipeName(Pipe p);

Pipe newPipe(char* name);

void deletePipe(Pipe p);

void getDescriptors(Pipe p, int** ret);

#endif
