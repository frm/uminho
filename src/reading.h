#ifndef READING_H_
#define READING_H_

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <strutil.h>

int read_file(char* filename);

char* getReadStats();
char* getAuthorStats();
int getAuthorsBy(char initial, char** list, int number_display, int* number_read);
void resetAuthorBy(char initial);
int getYearsTotalByInterval(int, int);
int *getYearsTotal(int *, int *);
int yieldYearCSV(char **);
void initializeGestauts();
void leaveGestauts();
int getYearCoAuthorsTotal(int, int);

#endif
