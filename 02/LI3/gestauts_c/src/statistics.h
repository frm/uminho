#ifndef STATISTICS_H_
#define STATISTICS_H_

/* initialize statistics */
void initializeStatistics(void);
/* delete statistics */
void deleteStatistics(void);
/* increment statics for a given number of coauthors in a given year */
void statsIncrement(int, int);
/* update statistics */
void statsUpdate(int, int, int);
/* get total of publications in a given year */
int statsGetYearTotal(int);
/* yield total of publications in a given year. returns 1 when the last year is yielded */
int statsYieldYearTotal(int *, int *);
/* yield year CSV. return 1 when the last year is yielded */
int statsYieldYearCSV(char **);
/* returns total number of publications with a given number of coauthors in a given year */
int statsGetYearCoAuthorsTotal(int, int);
#endif

