#ifndef AUTHOR_STATISTICS_H_
#define AUTHOR_STATISTICS_H_

char* getLongestAuthorName();
char* getShortestAuthorName();
double getAverage();
double getNumberAuthors();
int getMaxYear();
int getMinYear();
int getNrPublications();

void addToLength(int len);
void checkForYear(int new_year);
void checkForLength(char* author);

void initAuthorStats();
void deleteAuthorIndexStats();

#endif

