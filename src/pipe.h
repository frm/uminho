#ifndef PIPE_H
#define PIPE_H

typedef struct pipe *Pipe;

char* getPipeName(Pipe p);

Pipe newPipe(char* name, int fdesc[]);

void deletePipe(Pipe p);

#endif
