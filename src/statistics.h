#ifndef STATISTICS_H_
#define STATISTICS_H_

#include <string.h>
#include <stdio.h>
#include "statistics_tree.h"

void initializeStatistics(void);
void deleteStatistics(void);
void statsIncrement(int, int);
void statsUpdate(int, int, int);
int statsGetYearTotal(int);
int statsYieldYearTotal(int *, int *);
int statsYieldYearCSV(char **);
int statsGetYearCoAuthorsTotal(int, int);
#endif

