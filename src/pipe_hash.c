#include <string.h>
#include <stdlib.h>
#include <strutil.h>
#include "pipe.h"
#include "pipe_hash.h"

typedef struct node {
  Pipe content;
  struct node* next;
} *PipeBucket;

struct pipe_table {
  int size;
  PipeBucket* table;
};

/** Creates a hash bucket with an aggregation of given name and count total to 0
  * It shall not contain subaggregations
  */
static PipeBucket newPipeBucket(char* name) {
    PipeBucket new = (PipeBucket)malloc( sizeof (struct node) );
    new -> content = newPipe(name);
    new -> next = NULL;

    return new;
}


/* djb2 hash function created by Dan Bernstein */
static unsigned int hash( char *str ) {
   unsigned int hash = 5381;
    int c;

        while ( (c = *str++) )
            hash = ((hash << 5) + hash) ^ c;

        return hash;
}


/** Iterates the table looking for the aggregate with the given name
  * ret will contain the address of the bucket where it should be (whether or not it in fact is)
  * Return will be 1 or 0 depending on existance
  */
static int get_pipe_address(PipeTable a, char* name, PipeBucket** ret) {
    unsigned int index = hash(name) % a -> size;                    // Get table index
    int found = 0;                                                  // Control variable
    PipeBucket *it = &(a -> table)[index];                              // Iterator for that bucket
    PipeBucket *head = it;                                              // Saving the head of the bucket

    while (*it && !found) {                                         // Scanning the bucket for agregation
        if ( strcmp(name, getPipeName( (*it) -> content) ) == 0 )
            found = 1;
        else
            it = &( (*it) -> next );                                // Saving the new bucket address
    }

    if (! *it)
        *ret = head;                                                // Returning the position where the new bucket should be inserted
    else
        *ret = it;                                                  // Returning the position where the bucket is

    return found;                                                   // This allows us to know if we should create or update what's in ret
}

/** Tries to find the aggregate node
  * If found, returns its address
  * Otherwise adds it to the table
  */
static int get_pipe_ptr(PipeTable pt, char* name, PipeBucket** ret) {
    PipeBucket new;
    PipeBucket* it;
    int res = get_pipe_address(pt, name, &it);

    if(!res) {
        new = newPipeBucket(name);
        new -> next = *it;
        *it = new;
    }

    *ret = it;
    return res;
}

static void deletePipeBucket (PipeBucket b) {
    PipeBucket bird;                                // Auxiliary iterator that deletes everything behind
    PipeBucket it = b;                              // Main iterator that leads the way

    while (it) {
        bird = it;
        it = it -> next;
        deletePipe(bird -> content);
        free(bird);                             // badumm tss!
    }
}


void deletePipeTable(PipeTable pt) {
  if (pt) {
    for (int i = 0; i < pt -> size; i++)
        deletePipeBucket( pt -> table[i] );

    free(pt -> table);
    free(pt);
  }
}

PipeTable newPipeTable(int size) {
    PipeTable pt = (PipeTable)malloc(sizeof (struct pipe_table) );
    pt -> size = size;
    pt -> table = (PipeBucket*)calloc( size, sizeof(PipeBucket) );

    return pt;
}

int pipe_writer(PipeTable pt, char* name, int** ret) {
    if (!pt) return 0;
    PipeBucket* it;
    int res = get_pipe_ptr(pt, name, &it);
    getDescriptors( (*it) -> content, ret );
    return res;
}

void set_pid(PipeTable pt, char*name, int pid) {
  if (pt) {
    PipeBucket* it;
    get_pipe_ptr(pt, name, &it);
    setChildPid(pid, (*it) -> content);
  }
}

void shutdown_children(PipeTable pt) {
  for (int i = 0; i < (pt -> size); i++) {
    PipeBucket it = (pt -> table)[i];
    while (it) {
      closeChild(it -> content);
      it = it -> next;
    }
  }
}



char* bury_dead_child(PipeTable pt, int pid) {
  for (int i = 0; i < (pt -> size); i++) {
    PipeBucket it = (pt -> table)[i];
    PipeBucket b = it;

    while (it) {
      if( getChildPid(it -> content) == pid ) {
        char* name = str_dup( getPipeName(it -> content) );
        deletePipe(it -> content);
        if (it == (pt -> table)[i])   (pt -> table)[i] = it -> next;
        else                          b -> next = it -> next;
        free(it);
        return name;
      }

      b = it;
      it = it -> next;

    }
  }

  return NULL;
}

