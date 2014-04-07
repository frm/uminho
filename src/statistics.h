#ifndef STATISTICS_H
#define STATISTICS_H

#include <string.h>
#include "statistics_tree.h"

typedef YearStatsAVL StatsTree;
typedef CoAuthorStatsAVL CoAuthorStatsTree;

void initializeStatistics(void);
void deleteStatistics(void);
void statsIncrement(int, int);
void statsUpdate(int, int, int);
int statsGetYearTotal(int);
int statsYieldYearTotal(int *, int *);
int statsYieldYearCSV(char **);
int statsGetYearCoAuthorsTotal(int, int);
#endif

