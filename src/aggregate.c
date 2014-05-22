#include "agregate.h"

typedef struct agregate {
	char* name;
	int count;
	Agregation* subagregate;
} Agregate_s;

void countInc(Agregate ag, int val) {
	ag -> count += val;
}

void setName(Agregate ag, char* name) {
	ag -> name = strdup(name);
}

int getCount(Agregate ag) {
	return ag -> count;
}

char* getAgregateName(Agregate ag) {
	return ag -> name;
}

Agregate newAgregate() {
	return (Agregate)malloc( sizeof(Agregate_s) );
}

Agregate newAgregateWith(char* name, int count) {
	Agregate new = (Agregate)malloc( sizeof(Agregate_s) );
	new -> name = strdup(name);
	new -> count = count;
	new -> subagregate = NULL;
	return new;
}

/** MISSING: functions to add aggregation and subaggregation as well deleting */

