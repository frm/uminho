#ifndef PIPE_HASH_H
#define PIPE_HASH_H

typedef struct pipe_table *PipeTable;

/*Deletes the whole Pipe hash table*/
void deletePipeTable(PipeTable pt);

/*Creating a new Pipe Table*/
PipeTable newPipeTable(int size);

int pipe_writer(PipeTable pt, char* name, int** ret);

/**Sets the PID of the Pipe accessed by "name"*/
void set_pid(PipeTable pt, char*name, int pid);

/**Closes every Pipe in the given PipeTable*/
void shutdown_children(PipeTable pt);

char* bury_dead_child(PipeTable pt, int pid);

#endif
