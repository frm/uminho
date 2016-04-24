#ifndef AGGREGATE_H
#define AGGREGATE_H

#include <stdlib.h>
#include <string.h>
#include "../includes/strutil.h"

typedef struct aggregate_s* Aggregate;

#include "aggregation.h"

/** Increments counter with val */
void countInc(Aggregate ag, int val);

/** Sets the name of aggregate to given name */
void setName(Aggregate ag, char* name);

/** Returns the count of given aggregate */
int getCount(Aggregate ag);

/** Returns the name of the given aggregate */
char* getAggregateName(Aggregate ag);

/** Returns a new aggregate with no values */
Aggregate newAggregate();

/** Returns a new aggregate with given name and count */
Aggregate newAggregateWith(char* name, int count);

/** Deletes an aggregate */
void deleteAggregate(Aggregate a);

/** Returns pointer to subaggregation */
struct aggregation* getSubAggregate(Aggregate ag);

/** Returns 0 if doesn't have a subaggregation, 1 otherwise */
int hasSubAggregate(Aggregate ag);

/** Creates a subaggregation for given aggregate */
int createSubAggregate(Aggregate ag);

/** Creates an Aggregate allocating size for the SubAggregation */
Aggregate newAggregateFull(char* name, int count);

/** Increments an aggregate and its subaggregations */
int incrementAggregate(Aggregate a, char* name[], int count);

#ifdef DEBUG /** ### WARNING: DEBUG PURPOSES ONLY ### */
/** Prints aggregate and subaggregations */
void printAggregate(Aggregate ag);
#endif

#endif
