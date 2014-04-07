#ifndef STATISTICS_TREE_H
#define STATISTICS_TREE_H

#include <avl.h>

typedef struct CoAuthorStats_s{
    int coAuthors;
    int total; 
} CoAuthorStats;

AVL_DEF_HEADER(CoAuthorStats, int)

typedef struct YearStats_s{
    int year;
    int coAuthors[11];
    CoAuthorStatsAVL extraCoAuthors;
} YearStats;

AVL_DEF_HEADER(YearStats, int)

YearStatsAVL initStatsTree(void);
void deleteStatsTree(YearStatsAVL);
void statsTreeCoAuthorUpdate(YearStatsAVL, int, int, int);
int statsTreeGetYear(YearStatsAVL, int, YearStats *);
int statsTreeYield(YearStatsAVL, YearStats *);
int statsTreeFind(YearStatsAVL, int, YearStats *);
int coAuthorStatsTreeYield(CoAuthorStatsAVL, CoAuthorStats *);
int coAuthorStatsTreeFind(CoAuthorStatsAVL, int, CoAuthorStats *);
YearStats yearStatsClone(YearStats);
void yearStatsDestroy(YearStats);
int yearStatsComp(int *, YearStats *, YearStats);
void yearStatsCollision(YearStats *, YearStats *);
YearStats yearStatsNew();
void coAuthorStatsCollision(CoAuthorStats *, CoAuthorStats *);
int coAuthorStatsComp(int *, CoAuthorStats *, CoAuthorStats);

#endif

