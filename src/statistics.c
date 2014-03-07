#include "../headers/statistics.h"
#include "../headers/statistics_tree.h"

static StatsTree yearStatsTree = NULL;

void initializeStatistics() {
    yearStatsTree = initStatsTree();
}

void deleteStatistics() {
    deleteStatsTree(yearStatsTree);
}

static int getYearTotal(YearStats stats) {
    CoAuthorStats caStats;
    int i, test, total;

    total = 0;
    
    for (i = 1; i < 11; i++)
        total += stats.coAuthors[i];

    for (;;) {
        test = coAuthorStatsTreeYield(stats.extraCoAuthors, &caStats);

        if (!test) {
            total += caStats.total;
        }
        else {
            if (test == 1)
                total += caStats.total;

            break;
        }
    }

    return total;
}

void statsIncrement(int year, int coAuthors) {
    statsTreeCoAuthorUpdate(yearStatsTree, year, coAuthors, 1);
}

void statsUpdate(int year, int coAuthors, int total) {
    statsTreeCoAuthorUpdate(yearStatsTree, year, coAuthors, total);
}

int statsGetYearTotal(int year) {
    YearStats stats; 
    int total;

    total = 0;

    if (statsTreeFind(yearStatsTree, year, &stats))
        total = 0;
    else 
        total = getYearTotal(stats);

    yearStatsDestroy(stats);

    return total;
}

int statsYieldYearTotal(int *year, int *total) {
    YearStats stats;
    int ret;

    ret = statsTreeYield(yearStatsTree, &stats);

    *year = stats.year;
    *total = getYearTotal(stats);

    return ret;
}