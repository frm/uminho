#ifndef AGGREGATE_H
#define AGGREGATE_H

#include "aggregation.h"
#include <stdlib.h>
#include <string.h>


typedef struct aggregate_s *Aggregate;

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

#endif

