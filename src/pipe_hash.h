#ifndef PIPE_HASH_H
#define PIPE_HASH_H

typedef struct pipe_table *PipeTable;

void deletePipeTable(PipeTable pt);

PipeTable newPipeTable(int size);

#endif
