#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "statistics.h"
#include "statistics_tree.h"



static StatsTree yearStatsTree = NULL;

/* initialize statistics */
void initializeStatistics() {
    yearStatsTree = initStatsTree();
}

/* delete statistics */
void deleteStatistics() {
    deleteStatsTree(yearStatsTree);
}

/* get total of publications in a given year */
static int getYearTotal(YearStats stats) {
    CoAuthorStats caStats;
    int i, test, total;

    caStats = coAuthorStatsNew();

    total = 0;

    for (i = 1; i < 11; i++)
        total += yearStatsGetTotal(stats, i);

    do {
        test = yearStatsExtraYield(stats, caStats);

        if (test == 0 || test == 1)
            total += coAuthorStatsGetTotal(caStats);
    } while(!test);

    coAuthorStatsDestroy(caStats);

    return total;
}

/* increment statics for a given number of coauthors in a given year */
void statsIncrement(int year, int coAuthors) {
    statsTreeCoAuthorUpdate(yearStatsTree, year, coAuthors, 1);
}

/* update statistics */
void statsUpdate(int year, int coAuthors, int total) {
    statsTreeCoAuthorUpdate(yearStatsTree, year, coAuthors, total);
}

/* get total of publications in a given year */
int statsGetYearTotal(int year) {
    YearStats stats;
    int total;

    stats = yearStatsNew();

    total = 0;

    if (statsTreeFind(yearStatsTree, year, stats)){
        total = 0;
    }
    else {
        total = getYearTotal(stats);
    }

    yearStatsDestroy(stats);    

    return total;
}

/* yield total of publications in a given year. returns 1 when the last year is yielded */
int statsYieldYearTotal(int *year, int *total) {
    YearStats stats;
    int ret;

    stats = yearStatsNew();

    ret = statsTreeYield(yearStatsTree, stats);

    *year = yearStatsGetYear(stats);
    *total = getYearTotal(stats);

    yearStatsDestroy(stats);

    return ret;
}

/* returns a string with information about the number of coauthors in the given year
 * in CSV format. Parameter size is used to return the size of the string*/
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

/* returns total number of publications with a given number of coauthors in a given year */
int statsGetYearCoAuthorsTotal(int year, int coAuthors) {
    YearStats stats;
    int total;

    stats = yearStatsNew();

    if (statsTreeFind(yearStatsTree, year, stats))
        return 0;

    total = yearStatsGetTotal(stats, coAuthors);

    yearStatsDestroy(stats);

    return total;
}

/* yield year CSV. return 1 when the last year is yielded */
int statsYieldYearCSV(char **csv) {
    YearStats stats;
    CoAuthorStats caStats;
    int ret, i, index, year, size, total, test;
    char *yearCSV, *temp;

    stats = yearStatsNew();
    caStats = coAuthorStatsNew();

    size = 0;
    index = 0;
    test = 0;
    yearCSV = (char *)malloc(sizeof(char) * 1024);
    ret = statsTreeYield(yearStatsTree, stats);

    year = yearStatsGetYear(stats);

    for(i = 1; i < 11; i++) {
        total = yearStatsGetTotal(stats, i);
        if (total) {
            temp = getCoAuthCSV(year, i, total, &size);
            strncpy(yearCSV + index, temp, size);
            index += size;
            free(temp);
        }
    }

    do {
        test = yearStatsExtraYield(stats, caStats);

        if (test == 0 || test == 1) {
            temp = getCoAuthCSV(year, coAuthorStatsGetCoAuthors(caStats), coAuthorStatsGetTotal(caStats), &size);
            strncpy(yearCSV + index, temp, size);
            index += size;
            free(temp);
        }
    } while(!test);

    yearStatsDestroy(stats);
    coAuthorStatsDestroy(caStats);
    yearCSV[index] = '\0';

    *csv = yearCSV;

    return ret;
}
