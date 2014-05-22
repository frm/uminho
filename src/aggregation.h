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

#endif
