#ifndef PIPE_H
#define PIPE_H

typedef struct pipe *Pipe;

/**Retrieves the name of a pipe*/
char* getPipeName(Pipe p);

/**Creates a new "Pipe"*/
Pipe newPipe(char* name);

/**Deletes the "Pipe"*/
void deletePipe(Pipe p);

/**Retrieves the file descriptors in "Pipe p", writes them in the "ret" array*/
void getDescriptors(Pipe p, int** ret);

/**Retrieves the PID of the child process reading the pipe in "Pipe p"*/
int getChildPid(Pipe p);

/**Sets the child PID in "Pipe p"*/
void setChildPid(int pid, Pipe p);

/**Closes "Pipe p", writes 0 to it, telling the child the pipe is out of use*/
void closeChild(Pipe p);

#endif
