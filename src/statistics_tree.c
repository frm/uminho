#include "statistics_tree.h"

AVL_DEF(CoAuthorStats, int)

AVL_DEF(YearStats, int)
/*
 * CUSTOM AVL FUNCTION
 */

static void avlYearStatsCoAuthorUpdate(YearStatsAVL avl, int year, int coAuthors, int total) {
    YearStatsAVLNode node;
    YearStats stats;
    CoAuthorStats caStats;

    node = __avlYearStatsFind(avl->compare, avlGetRoot(avl), NULL, &year);

    if (!node) {
        stats = yearStatsNew();
        stats.year = year;
        if (coAuthors > 10) {
            caStats.coAuthors = coAuthors;
            caStats.total = total;
            avlInsert(CoAuthorStats, stats.extraCoAuthors, caStats);
        }
        else {
            stats.coAuthors[coAuthors] = total;
        }
        avlInsert(YearStats, avl, stats);
        yearStatsDestroy(stats);
    }
    else {
        stats = avlGetNodeContent(YearStats, node);
        if (coAuthors > 10) {
            caStats.coAuthors = coAuthors;
            caStats.total = total;
            avlInsert(CoAuthorStats, stats.extraCoAuthors, caStats);
        }
        else {
            stats.coAuthors[coAuthors] += total;
        }

        node->content = stats;
    }

    return;
}

YearStats yearStatsClone(YearStats stats) {
    YearStats newStats;
    int i;

    newStats.extraCoAuthors = avlClone(CoAuthorStats, stats.extraCoAuthors);

    for (i = 0; i < 11; i++) {
        newStats.coAuthors[i] = stats.coAuthors[i];
    }

    newStats.year = stats.year;

    return newStats;
}

void yearStatsDestroy(YearStats stats) {

    avlDestroy(CoAuthorStats, stats.extraCoAuthors);
}

int yearStatsComp(int *keyInt, YearStats *keyStats, YearStats stats) {
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

void yearStatsCollision(YearStats *stats1, YearStats *stats2) {
    CoAuthorStats temp;
    int end, i;

    end = 0;

    for (i = 0; i < 10; i++)
        stats1->coAuthors[i] += stats2->coAuthors[i];

    for(;;) {
        end = avlYield(CoAuthorStats, stats2->extraCoAuthors, &temp);

        if (!end){
            avlInsert(CoAuthorStats, stats1->extraCoAuthors, temp);
        }
        else {
            if (end == 1)
                avlInsert(CoAuthorStats, stats1->extraCoAuthors, temp);

            break;
        }
    }
}

YearStats yearStatsNew() {
    YearStats newStats;
    int i;

    newStats.extraCoAuthors = avlNewComplete(CoAuthorStats, &coAuthorStatsComp, &coAuthorStatsCollision, NULL, NULL);
    for (i = 0; i < 11; i++)
        newStats.coAuthors[i] = 0;

    return newStats;
}

void coAuthorStatsCollision(CoAuthorStats *stats1, CoAuthorStats *stats2) {
    stats1->total += stats2->total;
}

int coAuthorStatsComp(int *keyInt, CoAuthorStats *keyStats, CoAuthorStats stats) {
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

YearStatsAVL initStatsTree() {
    return avlNewComplete(YearStats, &yearStatsComp, &yearStatsCollision, &yearStatsDestroy, &yearStatsClone);
}

void deleteStatsTree(YearStatsAVL yearStatsTree) {
    avlDestroy(YearStats, yearStatsTree);
}

int statsTreeGetYear(YearStatsAVL yearStatsTree, int year, YearStats *ret) {
    return avlFind(YearStats, yearStatsTree, year, ret);
}

int statsTreeYield(YearStatsAVL yearStatsTree, YearStats *ret) {
    return avlYield(YearStats, yearStatsTree, ret);
}

void statsTreeCoAuthorUpdate(YearStatsAVL yearStatsTree, int year, int coAuthors, int total) {
    avlYearStatsCoAuthorUpdate(yearStatsTree, year, coAuthors, total);
}

int statsTreeFind(YearStatsAVL yearStatsTree, int year, YearStats *ret) {
    return avlFind(YearStats, yearStatsTree, year, ret);
}

int coAuthorStatsTreeYield(CoAuthorStatsAVL coAuthorStatsTree, CoAuthorStats *ret) {
    return avlYield(CoAuthorStats, coAuthorStatsTree, ret);
}

int coAuthorStatsTreeFind(CoAuthorStatsAVL coAuthorStatsTree, int coAuthors, CoAuthorStats *ret) {
    return avlFind(CoAuthorStats, coAuthorStatsTree, coAuthors, ret);
}
