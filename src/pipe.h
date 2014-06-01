#ifndef PIPE_H
#define PIPE_H

typedef struct pipe *Pipe;

char* getPipeName(Pipe p);

Pipe newPipe(char* name);

void deletePipe(Pipe p);

void getDescriptors(Pipe p, int** ret);

pid_t getChildPid(Pipe p);

void setChildPid(pid_t pid, Pipe p);

void closeChild(Pipe p);

#endif
