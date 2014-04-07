#include "statistics.h"
#include "statistics_tree.h"

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

    if (statsTreeFind(yearStatsTree, year, &stats)){
        total = 0;
    }
    else {
        total = getYearTotal(stats);
        yearStatsDestroy(stats);
    }

    return total;
}

int statsYieldYearTotal(int *year, int *total) {
    YearStats stats;
    int ret;

    ret = statsTreeYield(yearStatsTree, &stats);

    *year = stats.year;
    *total = getYearTotal(stats);

    yearStatsDestroy(stats);

    return ret;
}

static char *getCoAuthCSV(int year, int coAuthors, int total, int *size) {
    int index;
    char *temp;

    index = 0;
    temp = (char *)malloc(sizeof(char) * 256);

    sprintf(temp, "\"%d\",\"%d\",\"%d\"\n", year, coAuthors, total);

    index += 9;

    do {
        index++;
    } while (year /= 10);

    do {
        index++;
    } while(total /= 10);

    do {
        index++;
    } while(coAuthors /= 10);

    *size = index;

    return temp;
}

int statsGetYearCoAuthorsTotal(int year, int coAuthors) {
    YearStats stats;
    CoAuthorStats caStats;
    int total;

    if (statsTreeFind(yearStatsTree, year, &stats))
        return 0;
    
    if (coAuthors < 11){
        total = stats.coAuthors[coAuthors];
    }
    else {
        if (coAuthorStatsTreeFind(stats.extraCoAuthors, coAuthors, &caStats))
            return 0;
        
        total = caStats.total; 
    }

    yearStatsDestroy(stats);

    return total;
}

int statsYieldYearCSV(char **csv) {
    YearStats stats;
    CoAuthorStats caStats;
    int ret, i, index, year, size, total, test;
    char *yearCSV, *temp;

    size = 0;
    index = 0;
    test = 0;
    yearCSV = (char *)malloc(sizeof(char) * 1024);
    ret = statsTreeYield(yearStatsTree, &stats);

    year = stats.year;

    for(i = 1; i < 11; i++) {
        total = stats.coAuthors[i];
        if (total) {
            temp = getCoAuthCSV(year, i, total, &size);
            strncpy(yearCSV + index, temp, size);
            index += size;
            free(temp);
        }
    }

    for (;;) {
        test = coAuthorStatsTreeYield(stats.extraCoAuthors, &caStats);

        if (!test) {
            temp = getCoAuthCSV(year, caStats.coAuthors, caStats.total, &size);
            strncpy(yearCSV + index, temp, size);
            index += size;
            free(temp);
        }
        else {
            if (test == 1) {
                temp = getCoAuthCSV(year, caStats.coAuthors, caStats.total, &size);
                strncpy(yearCSV + index, temp, size);
                index += size;
                free(temp);
            }

            break;
        }
    }

    yearCSV[index] = '\0';

    *csv = yearCSV;

    return ret;
}