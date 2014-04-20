#ifndef READING_H_
#define READING_H_

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <strutil.h>

int read_file(char* filename);

char* getReadStats();
char* getAuthorStats();
int getAuthorsBy(char, char**, int, int*);
void resetAuthorBy(char);
int getYearsTotalByInterval(int, int);
int *getYearsTotal(int *, int *);
int yieldYearCSV(char **);
void initializeGestauts();
void leaveGestauts();
int getYearCoAuthorsTotal(int, int);
char **topAuthorsInYear(int, int);
int authorPublicationsInYear(char *, int);
float getAuthorYearRatio(char *, int);
int totalSoloAuthors();
int** getAuthorPublicationsByYear(char* author, int* size);
void deleteAuthorPublicationsMatrix(int** matrix);

#endif
