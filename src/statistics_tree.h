#ifndef STATISTICS_TREE_H
#define STATISTICS_TREE_H

typedef struct YearStats_s *YearStats;
typedef struct CoAuthorStats_s *CoAuthorStats;

typedef struct YearStatsContAVL_s *StatsTree;


YearStats yearStatsNew();
void yearStatsDestroy(YearStats);
YearStats yearStatsClone(YearStats);
int yearStatsGetYear(YearStats stats);

CoAuthorStats coAuthorStatsNew();
void coAuthorStatsDestroy(CoAuthorStats);
int coAuthorStatsGetTotal(CoAuthorStats stats);
int coAuthorStatsGetCoAuthors(CoAuthorStats stats);

/* Initialize statistics tree */
StatsTree initStatsTree(void);
/* Destroy statistics tree */
void deleteStatsTree(StatsTree);

/* Update statistics about a given number of coauthors */
void statsTreeCoAuthorUpdate(StatsTree, int, int, int);
/* Get information about an year. Returns 0 if successful */
int statsTreeGetYear(StatsTree, int, YearStats);
/* Yield information about an year from the StatsTree.
 * Returns 1 if the last year is yielded, -1 if the tree is empty, 0 otherwise*/
int statsTreeYield(StatsTree, YearStats);
/* Find information about a given year. Returns 0 if successful */
int statsTreeFind(StatsTree, int, YearStats);
/* Returns the total of publications with a given number of coauthors in a given year */
int yearStatsGetTotal(YearStats stats, int coauthors);
/* Yields from the tree used to store information about publications with > 10 coauthors */
int yearStatsExtraYield(YearStats stats, CoAuthorStats ret);


#endif

