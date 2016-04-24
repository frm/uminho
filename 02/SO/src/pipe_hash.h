#ifndef PIPE_HASH_H
#define PIPE_HASH_H

typedef struct pipe_table *PipeTable;

/*Deletes the whole Pipe hash table*/
void deletePipeTable(PipeTable pt);

/*Creating a new Pipe Table*/
PipeTable newPipeTable(int size);

/** Checks if a district exists, creating it if so.
  * ret shall contain the file descriptors created for father-child communication
  * Returns 1 if needed to create a new father-son Pipe structure
  * Returns 0 if it already existed
  */
int pipe_writer(PipeTable pt, char* name, int** ret);

/**Sets the PID of the Pipe accessed by "name"*/
void set_pid(PipeTable pt, char*name, int pid);

/**Closes every Pipe in the given PipeTable*/
void shutdown_children(PipeTable pt);

/** Iterates the hash searching for the element with given pid
  * Removes it if found. In that case returns the name of the Pipe structure
  * Otherwise returns NULL
  */
char* bury_dead_child(PipeTable pt, int pid);

#endif
