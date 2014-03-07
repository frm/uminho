#ifndef STATISTICS_TREE_H
#define STATISTICS_TREE_H

#include "../../lib/headers/avl.h"

typedef struct CoAuthorStats_s{
    int coAuthors;
    int total; 
} CoAuthorStats;

AVL_DEF_HEADER(CoAuthorStats, int);

typedef struct YearStats_s{
    int year;
    int coAuthors[11];
    CoAuthorStatsAVL extraCoAuthors;
} YearStats;

AVL_DEF_HEADER(YearStats, int);

YearStatsAVL initStatsTree(void);
void deleteStatsTree(YearStatsAVL);
void statsTreeCoAuthorUpdate(YearStatsAVL, int, int, int);
int statsTreeGetYear(YearStatsAVL, int, YearStats *);
int statsTreeYield(YearStatsAVL, YearStats *);
int statsTreeFind(YearStatsAVL, int, YearStats *);
int coAuthorStatsTreeYield(CoAuthorStatsAVL, CoAuthorStats *);

YearStats yearStatsClone(YearStats stats);
void yearStatsDestroy(YearStats stats);
int yearStatsComp(int *keyInt, YearStats *keyStats, YearStats stats);
void yearStatsCollision(YearStats *stats1, YearStats *stats2);
YearStats yearStatsNew();
void coAuthorStatsCollision(CoAuthorStats *stats1, CoAuthorStats *stats2);
int coAuthorStatsComp(int *keyInt, CoAuthorStats *keyStats, CoAuthorStats stats);

#endif