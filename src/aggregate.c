#include "aggregate.h"

struct aggregate_s {
	char* name;
	int count;
	Aggregation subaggregate;
};

void countInc(Aggregate ag, int val) {
	ag -> count += val;
}

void setName(Aggregate ag, char* name) {
	ag -> name = strdup(name);
}

int getCount(Aggregate ag) {
	return ag -> count;
}

char* getAggregateName(Aggregate ag) {
	return ag -> name;
}

Aggregate newAggregate() {
	return (Aggregate)malloc( sizeof(struct aggregate_s) );
}

Aggregate newAggregateWith(char* name, int count) {
	Aggregate new = (Aggregate)malloc( sizeof(struct aggregate_s) );
	new -> name = strdup(name);
	new -> count = count;
	new -> subaggregate = NULL;
	return new;
}

void deleteAggregate(Aggregate a) {
	free(a -> name);
	deleteAggregation(a -> subaggregate);
	free(a);
}

/** MISSING: functions to add aggregation and subaggregation as well deleting */

