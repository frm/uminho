#include "aggregation.h"
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <fcntl.h>

typedef struct bucket_node Node, *Bucket;

struct bucket_node {
  Aggregate content;
  struct bucket_node* next;
};

struct aggregation {
  int size;
  Bucket* table;
};

/** Creates a hash bucket with an aggregation of given name and count total to 0
  * It shall not contain subaggregations
  */
static Bucket newBucket(char* name) {
    Bucket new = (Bucket)malloc( sizeof (struct bucket_node) );
    new -> content = newAggregateWith(name, 0);
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
static int get_bucket_address(Aggregation a, char* name, Bucket** ret) {
    unsigned int index = hash(name) % a -> size;                    // Get table index
    int found = 0;                                                  // Control variable
    Bucket *it = &(a -> table)[index];                              // Iterator for that bucket
    Bucket *head = it;                                              // Saving the head of the bucket

    while (*it && !found) {                                         // Scanning the bucket for agregation
        if ( strcmp(name, getAggregateName( (*it) -> content) ) == 0 )
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
static Bucket* get_aggregate_ptr(Aggregation a, char* name) {
    Bucket new;
    Bucket* it;

    if (! get_bucket_address(a, name, &it) ) {
        new = newBucket(name);
        new -> next = *it;
        *it = new;
    }

    return it;
}

static Aggregate getAggregate(Aggregation a, char* name) {
    Bucket* b;
    if(! get_bucket_address(a, name, &b) ) return NULL;
    else return (*b) -> content;
}

static void deleteBucket (Bucket b) {
    Bucket bird;                                // Auxiliary iterator that deletes everything behind
    Bucket it = b;                              // Main iterator that leads the way
    while (it) {
        bird = it;
        it = it -> next;
        deleteAggregate(bird -> content);
        free(bird);                             // badumm tss!
    }
}

void deleteAggregation(Aggregation a) {
  if (a) {
    for (int i = 0; i < a -> size; i++)
        deleteBucket( a -> table[i] );

    free(a -> table);
    free(a);
  }
}

/** Creates a new aggregation with given size */
Aggregation newAggregation(int size) {
    Aggregation a = (Aggregation)malloc(sizeof (struct aggregation) );
    a -> size = size;
    a -> table = (Bucket*)calloc( size, sizeof(Bucket) );

    return a;
}


/** Updates an Aggregation, incrementing te given value. If the given name does not exist, it will be created */
int updateAggregation(Aggregation a, char *name[], int count) {

    if (*name) {
        Aggregate curr = ( *get_aggregate_ptr(a, *name) ) -> content;
        countInc(curr, count);
        int res = createSubAggregate(curr);
        updateAggregation( getSubAggregate(curr), name + 1, count );
        return res;
    }

    return -1;
}

/** Aux function that turns a count int to string to be written */
static char* get_count_str(Aggregate a) {
    char* count = (char*)malloc(sizeof(char) * 10);
    sprintf( count, "%d\n", getCount(a) );
    return count;
}

/** Aux function that updates the path the aggregation has until that aggregate */
static char* get_new_path(char* path, char* add) {
    char* new_path = (char*)calloc( strlen(add) + strlen(path) + 2, sizeof(char) );
    sprintf(new_path, "%s:%s", path, add);
    return new_path;
}

// Necessary header declaration
static int aggregate_level(Aggregation a, int level, char* filename, char* path, char* last);

/** Iterates over every aggregate in a subaggregation, trying to collect its count */
static int update_subs(Aggregation a, int level, char* path, char* filename) {
    int res = -1;
    for (int i = 0; i < a -> size; i++) {
        Bucket b = (a -> table)[i];

        while (b) {
            char* name = getAggregateName(b -> content);
            char* new_path = get_new_path(path, name);
            if ( aggregate_level( a, level - 1, filename, new_path, name ) == 0)
                res = 0;
            free(new_path);
            b = b -> next;
        }
    }
    return res;
}

/** Aux function that writes a path and count to given file */
static void write_to_file(char* filename, char* path, char* count) {
		char logfile[1024];
		sprintf(logfile, "%s.dat", filename);
		int fd = open(logfile, O_CREAT | O_WRONLY | O_APPEND, 0666);

		write( fd, path, strlen(path) );
    	write( fd, ":", sizeof(char) );

		write( fd, count, strlen(count) );

        close(fd);
}

/** Writes the total count to a file */
static void write_total_to_file(char* filename, char* path, char* count) {
    char* total = get_new_path(" \tTotal", path); // same function call, inverted arguments
    write_to_file(filename, total, count);
    free(total);
}

/** Writes the given aggregate to a file */
static void write_aggregate(Aggregate a, char* filename, char* path) {
    char* count = get_count_str(a);
    write_to_file(filename, path, count);
    free(count);
}

/** Writes the total count of an aggregate to a file */
static void write_total_aggregate(Aggregate a, char* filename, char* path) {
    char* count = get_count_str(a);
    write_total_to_file(filename, path, count);
    free(count);
}

/** Collects all elements in a level recursively until it is 0 */
static int aggregate_level(Aggregation a, int level, char* filename, char* path, char* last) {
	if (!a) return -1;

    Aggregate curr_ag = getAggregate(a, last);

	if (level == 0)
        write_aggregate(curr_ag, filename, path);

	else {
        Aggregation sub = getSubAggregate(curr_ag);
        if ( update_subs(sub, level, path, filename) == 0 )
            write_total_aggregate(curr_ag, filename, path);
	}

	return 0;
}

/** Descends in an aggregation, trying to collect it. If has reached end has levels below, starts descending on levels */
static int aggregate_descend(Aggregation a, char* name[], int level, char* filename, char* path ) {
    if( ! *name ) return -1;

    Aggregate curr_ag = getAggregate(a, *name);

    if (!curr_ag) return -1;

    sprintf(path, "%s:%s", path, *name);

    if ( *(name + 1) == NULL )
		return aggregate_level(a, level, filename, path, *name);


	return aggregate_descend( getSubAggregate(curr_ag), name + 1, level, filename, path );
}

/** Collects an aggregate. First descends in the given name until it is NULL.
  * If it still has levels to descend, it does so for all elements in the level
  * Returns 0 if was able to collect
  * Returns -1 if the arguments were invalid (e.g. inexisting levels)
  */
int collectAggregate(Aggregate a, char* name[], int level, char* filename) {
	if ( !name || !a )
		return -1;

	char* path = (char*)calloc(1024, sizeof(char));
    int size = strlen(name[0]) + 1;
    memcpy(path, name[0], size);
	int res;

    if( !name[1] ) {

        if(level == 0)
            write_aggregate(a, filename, path);

        else if ( update_subs(getSubAggregate(a), level, path, filename) == 0 )
            write_total_aggregate(a, filename, path);

        res = 0;
    }

    else res = aggregate_descend( getSubAggregate(a), name + 1, level, filename, path);
    free(path);

    return res;
}



#ifdef DEBUG
void printAggregation(Aggregation a) {
	if (!a) return;
	for (int i = 0; i < a -> size; i++) {
		for (Bucket b = (a -> table)[i]; b; b = b -> next)
			printAggregate(b -> content);
	}
}
#endif

