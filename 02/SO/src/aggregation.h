#ifndef AGGREGATION_H
#define AGGREGATION_H

#include <string.h>
#include "aggregate.h"

#define AGGREGATION_SIZE        500


typedef struct aggregation* Aggregation;

/** Creates an aggregation with the given size */
Aggregation newAggregation(int size);

/** Deletes a given aggregation */
void deleteAggregation(Aggregation a);

/** Updates an aggregation incrementing its count and consequently subaggregations count */
int updateAggregation(Aggregation a, char *name[], int count);

/** Writes the corresponding level of the given aggregation to filename */
int collectAggregate(struct aggregate_s * a, char* name[], int level, char* filename);

#ifdef DEBUG /** ### WARNING: DEBUG PURPOSES ONLY ### */
/** Prints the aggregation */
void printAggregation(Aggregation a);
#endif

#endif
