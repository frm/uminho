#include "aggregate.h"

/** Aggregate structure definition */
struct aggregate_s {
	char* name;					// Aggregate name
	int count;					// Aggregate count
	Aggregation subaggregate;	// Subaggregate
};

/** Updates the count for a given aggregate with val */
void countInc(Aggregate ag, int val) {
	ag -> count += val;
}

/** Sets the given name for a given aggregate. No spaces allowed on ends. Name will be trimmed */
void setName(Aggregate ag, char* name) {
	ag -> name = strtrim(name);
}

/** Returns the count of the corresponding aggregate */
int getCount(Aggregate ag) {
	return ag -> count;
}

/** Returns the name of the corresponding aggregate */
char* getAggregateName(Aggregate ag) {
	return ag -> name;
}

/** Returns a pointer to the subaggregate */
Aggregation getSubAggregate(Aggregate ag) {
	return ag -> subaggregate;
}

/** Returns true if the Aggregate has a subaggregate */
int hasSubAggregate(Aggregate ag) {
	return ag -> subaggregate != NULL;
}

/** Creates a subaggregate with AGGREGATION_SIZE */
int createSubAggregate(Aggregate ag) {
	int res = 0;

	if ( !hasSubAggregate(ag) ) {
		ag -> subaggregate = newAggregation(AGGREGATION_SIZE);
		res = 1;
	}

	return res;
}

/** Allocs memory for a new Aggregate */
Aggregate newAggregate() {
	return (Aggregate)malloc( sizeof(struct aggregate_s) );
}

/** Creates a new Aggregate with given attributes but does not allocate memory for a subaggregate */
Aggregate newAggregateWith(char* name, int count) {
	Aggregate new = (Aggregate)malloc( sizeof(struct aggregate_s) );
	new -> name = strtrim(name);
	new -> count = count;
	new -> subaggregate = NULL;
	return new;
}

/** Creates a new Aggregate with given attributes and allocates memory for a subaggregate */
Aggregate newAggregateFull(char* name, int count) {
	Aggregate new = newAggregateWith(name, count);
	createSubAggregate(new);
	return new;
}

/** Deletes the given Aggregate */
void deleteAggregate(Aggregate a) {
	free(a -> name);
	deleteAggregation(a -> subaggregate);
	free(a);
}

/** Increments a given aggregate and the corresponding subaggregate */
int incrementAggregate(Aggregate a, char* name[], int count) {
	if( !name || !a || !*name || strcmp(a -> name, *name) ) return -1;
	countInc(a, count);
	return updateAggregation(a -> subaggregate, name + 1, count);
}

#ifdef DEBUG

#include <stdio.h>
void printAggregate(Aggregate a) {
	printf("AGGREGATE NAME: %s\n", a -> name);
	printf("TOTAL COUNT: %d\n", a -> count);
	printf("SUBAGGREGATIONS:\n");
	printAggregation(a -> subaggregate);
	printf("\n");
}

#endif
