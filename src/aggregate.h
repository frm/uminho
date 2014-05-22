#ifndef AGREGATE_H
#define AGREGATE_H

#include <stdlib.h>
#include <string.h>

typedef struct agregate *Agregate;

/** Increments counter with val */
void countInc(Agregate ag, int val);

/** Sets the name of ag to name */
void setName(Agregate ag, char* name);

/** Returns the count of given agregate */
int getCount(Agregate ag);

/** Returns the name of the given agregate */
char* getAgregateName(Agregate ag);

/** Returns a new agregate with no values */
Agregate newAgregate();

/** Returns a new agregate with given name and count */
Agregate newAgregateWith(char* name, int count);

#endif

