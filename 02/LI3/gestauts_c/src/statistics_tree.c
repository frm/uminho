#include "statistics_tree.h"

#include <stdlib.h>
#include <avl.h>
#include <stdio.h>

typedef struct CoAuthorStatsContAVL_s *CoAuthorStatsTree;

/*stores information about coauthors */
typedef struct CoAuthorStats_s{
    int coAuthors;
    int total; 
} CoAuthorStatsCont;

AVL_DEF_HEADER(CoAuthorStatsCont, int)

/*stores information about coauthors related to a year */
typedef struct YearStats_s{
    int year;
    int coAuthors[11];
    CoAuthorStatsContAVL extraCoAuthors;
} YearStatsCont;

AVL_DEF_HEADER(YearStatsCont, int)

AVL_DEF(CoAuthorStatsCont, int)

AVL_DEF(YearStatsCont, int)

static void coAuthorStatsContCollision(CoAuthorStatsCont *stats1, CoAuthorStatsCont *stats2) {
    stats1->total += stats2->total;
}

static int coAuthorStatsContComp(int *keyInt, CoAuthorStatsCont *keyStats, CoAuthorStatsCont stats) {
    int key;

    if (keyInt)
        key = *keyInt;
    else
        key = keyStats->coAuthors;

    if (key > stats.coAuthors)
        return 1;
    else if (key < stats.coAuthors)
        return -1;
    else
        return 0;
}

CoAuthorStats coAuthorStatsNew() {
    CoAuthorStats new;

    new = (CoAuthorStats)malloc(sizeof(CoAuthorStatsCont));

    return new;
}

void coAuthorStatsDestroy(CoAuthorStats stats) {
    free(stats);
}

static YearStatsCont yearStatsContNew() {
    YearStatsCont newStats;
    int i;

    newStats.extraCoAuthors = avlNewComplete(CoAuthorStatsCont, &coAuthorStatsContComp, &coAuthorStatsContCollision, NULL, NULL);
    for (i = 0; i < 11; i++)
        newStats.coAuthors[i] = 0;

    return newStats;
}

YearStats yearStatsNew() {
    YearStats newStats;

    newStats = (YearStats)malloc(sizeof(YearStatsCont));
    newStats->extraCoAuthors = NULL;

    return newStats;
}

static void yearStatsContDestroy(YearStatsCont stats) {
    if (stats.extraCoAuthors)
        avlDestroy(CoAuthorStatsCont, stats.extraCoAuthors);
}

void yearStatsDestroy(YearStats stats) {
    yearStatsContDestroy(*stats);
    free(stats);
}


/*
 * CUSTOM AVL FUNCTION
 */

static void avlYearStatsCoAuthorUpdate(StatsTree avl, int year, int coAuthors, int total) {
    YearStatsContAVLNode node;
    YearStatsCont stats;
    CoAuthorStatsCont caStats;

    node = __avlYearStatsContFind(avl->compare, avlGetRoot(avl), NULL, &year);

    if (!node) {
        stats = yearStatsContNew();
        stats.year = year;
        if (coAuthors > 10) {
            caStats.coAuthors = coAuthors;
            caStats.total = total;
            avlInsert(CoAuthorStatsCont, stats.extraCoAuthors, caStats);
        }
        else {
            stats.coAuthors[coAuthors] = total;
        }
        avlInsert(YearStatsCont, avl, stats);
        yearStatsContDestroy(stats);
    }
    else {
        stats = avlGetNodeContent(YearStatsCont, node);
        if (coAuthors > 10) {
            caStats.coAuthors = coAuthors;
            caStats.total = total;
            avlInsert(CoAuthorStatsCont, stats.extraCoAuthors, caStats);
        }
        else {
            stats.coAuthors[coAuthors] += total;
        }

        node->content = stats;
    }
    return;
}

static YearStatsCont yearStatsContClone(YearStatsCont stats) {
    YearStatsCont newStats;
    int i;

    newStats.extraCoAuthors = avlClone(CoAuthorStatsCont, stats.extraCoAuthors);

    for (i = 0; i < 11; i++) {
        newStats.coAuthors[i] = stats.coAuthors[i];
    }

    newStats.year = stats.year;

    return newStats;
}

static int yearStatsContComp(int *keyInt, YearStatsCont *keyStats, YearStatsCont stats) {
    int key = 0;

    if (keyInt)
        key = *keyInt;
    else
        key = keyStats->year;

    if (key > stats.year)
        return 1;
    else if (key < stats.year)
        return -1;
    else 
        return 0;

}

static void yearStatsContCollision(YearStatsCont *stats1, YearStatsCont *stats2) {
    CoAuthorStatsCont temp;
    int end, i;

    end = 0;

    for (i = 0; i < 10; i++)
        stats1->coAuthors[i] += stats2->coAuthors[i];

    for(;;) {
        end = avlYield(CoAuthorStatsCont, stats2->extraCoAuthors, &temp);

        if (!end){
            avlInsert(CoAuthorStatsCont, stats1->extraCoAuthors, temp);
        }
        else {
            if (end == 1)
                avlInsert(CoAuthorStatsCont, stats1->extraCoAuthors, temp);

            break;
        }
    }
}


int yearStatsGetYear(YearStats stats) {
    return stats->year;
}
/* Returns the total of publications with a given number of coauthors in a given year */
int yearStatsGetTotal(YearStats stats, int coauthors) {
    CoAuthorStatsCont ret;

    if (coauthors <= 10)
        return stats->coAuthors[coauthors];
    else
        if (!avlFind(CoAuthorStatsCont, stats->extraCoAuthors, coauthors, &ret))
            return ret.total;
        else
            return 0;
}

int coAuthorStatsGetCoAuthors(CoAuthorStats stats) {
    return stats->coAuthors;
}

int coAuthorStatsGetTotal(CoAuthorStats stats) {
    return stats->total;
}

int coAuthorStatsTreeYield(CoAuthorStatsTree tree, CoAuthorStats ret) {
    return avlYield(CoAuthorStatsCont, tree, ret);
}

/* Yields from the tree used to store information about publications with > 10 coauthors */
int yearStatsExtraYield(YearStats stats, CoAuthorStats ret) {
    return coAuthorStatsTreeYield(stats->extraCoAuthors, ret);
}


/* Initialize statistics tree */
StatsTree initStatsTree() {
    StatsTree new;

    new = avlNewComplete(YearStatsCont, &yearStatsContComp, &yearStatsContCollision, &yearStatsContDestroy, &yearStatsContClone);

    return new;
}

/* Destroy statistics tree */
void deleteStatsTree(StatsTree yearStatsTree) {
    avlDestroy(YearStatsCont, yearStatsTree);
}

/* Get information about an year. Returns 0 if successful */
int statsTreeGetYear(StatsTree yearStatsTree, int year, YearStats ret) {
    return avlFind(YearStatsCont, yearStatsTree, year, ret);
}

/* Yield information about an year from the StatsTree.
 * Returns 1 if the last year is yielded, -1 if the tree is empty, 0 otherwise*/
int statsTreeYield(StatsTree yearStatsTree, YearStats ret) {
    return avlYield(YearStatsCont, yearStatsTree, ret);
}

/* Update statistics about a given number of coauthors */
void statsTreeCoAuthorUpdate(StatsTree yearStatsTree, int year, int coAuthors, int total) {
    avlYearStatsCoAuthorUpdate(yearStatsTree, year, coAuthors, total);
}

/* Find information about a given year. Returns 0 if successful */
int statsTreeFind(StatsTree yearStatsTree, int year, YearStats ret) {
    return avlFind(YearStatsCont, yearStatsTree, year, ret);
}

int coAuthorStatsTreeFind(CoAuthorStatsTree tree, int coAuthors, CoAuthorStats ret) {
    return avlFind(CoAuthorStatsCont, tree, coAuthors, ret);
}