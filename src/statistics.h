#ifndef STATISTICS_H
#define STATISTICS_H

#include "statistics_tree.h"

typedef YearStatsAVL StatsTree;
typedef CoAuthorStatsAVL CoAuthorStatsTree;

void initializeStatistics(void);
void deleteStatistics(void);
void statsIncrement(int, int);
void statsUpdate(int, int, int);
int statsGetYearTotal(int);
int statsYieldYearTotal(int *, int *);

#endif