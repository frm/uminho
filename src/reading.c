#include "reading.h"
#include "author_index.h"
#include "statistics.h"
#include "author_catalog.h"

#define getMatrixAuthorIndex(author, i)		(author + 128 * i)

static char* fileread;

static void extract_year_info(char* year_str, int coAuthors) {
	/* Function that should use year to complete statistics */
	int year = atoi(year_str);
	checkForYear(year);
	statsIncrement(year, coAuthors);
}

static void extract_author_info(char* author) {
	/* Function that should:
	 * add author to author_index
	 * use author info to calculate statistics
	 */
	int insertion = insertAuthor(author);

	if (insertion == 0) {
		addToLength( strlen(author) );
		checkForLength(author);
	}
}

static int isAuthor(char* str) { return !isdigit(str[0]); }

static void tokenize(char* buffer) {
	char** author_buffer = (char**)malloc( sizeof(char) * 128 );
	char* token = strtrim( strtok(buffer, ",") );
	int n = 0;

	while (token) {
		/* use !isdigit() instead of isalpha because of names started with special characters */
		/* use a function for this line */
		author_buffer[n] = (char*)malloc(sizeof(char) * ( strlen(token) + 1 ) );
		strncpy(author_buffer[n], token, sizeof(char) * ( strlen(token) + 1 ) );
		
		if ( isAuthor(token) ) {
			extract_author_info(token);
			n++;
		}
		else {
			extract_year_info(token, n);
		}

		/* strtrim is allocating mem
		 * since we're making copies into the tree, no need for dupmem
		 */
		free(token);
		token = strtrim( strtok(NULL, ",") );
	}
	
	insertToCatalog(author_buffer, n);
	while ( n > 0 )
		free( author_buffer[n--] );

	free(author_buffer);

}

static void set_filename(char* filename) {
	fileread = (char*)malloc(sizeof(char) * ( strlen(filename) + 1 ) );
	strncpy(fileread, filename, sizeof(char) * ( strlen(filename) + 1 ) );
}


int read_file(char* filename) {
	char buffer[1024];
	FILE *file = fopen(filename, "r");

	set_filename(filename);

	/* ERROR HANDLING */
	if (!file)
		return -1;

	while( fgets(buffer, 1024, file) ) {
		tokenize(buffer);
	}

	fclose(file);

	return 0;
}

char* getReadStats() {
	char* stats = (char*)malloc(sizeof(char) * 1024);

	sprintf( stats, "\nFILE READ: %s\nTOTAL NUMBER OF PUBLICATIONS: %d\nTOTAL NUMBER OF AUTHORS:%d\n", fileread, getNrPublications(), (int)getNumberAuthors() );
	sprintf( stats + strlen(stats), "MIN YEAR:%d\nMAX YEAR:%d\n", getMinYear(), getMaxYear() );

	return stats;
}

char* getAuthorStats() {
	char* stats = (char*)malloc(sizeof(char) * 1024);

	sprintf( stats, "\nLONGEST NAME: %s with length %d\n", getLongestAuthorName(), (int)strlen( getLongestAuthorName() ) );
	sprintf( stats + strlen(stats), "SHORTEST NAME: %s with length %d\n", getShortestAuthorName(), (int)strlen( getShortestAuthorName() ) );
	sprintf( stats + strlen(stats), "AVERAGE LENGTH: %f\n", getAverage() );

	return stats;
}

int *getYearsTotal(int *minYear, int *maxYear) {
	int min, max, year, totalYears, totalPubs, test;
	int *totals;

	min = getMinYear();
	max = getMaxYear();
	totalYears = max - min + 1;
	totals = (int *)calloc(totalYears, sizeof(int));

	for (;;) {
		test = statsYieldYearTotal(&year, &totalPubs);

		if (!test){
			totals[year - min] = totalPubs;
		}
		else {
			if (test == 1)
				totals[year - min] = totalPubs;

			break;
		}
	}

	*minYear = min;
	*maxYear = max;
	return totals;
}

int getYearsTotalByInterval(int first, int last) {
	int min, max, i, total;

	total = 0;
	min = getMinYear();
	max = getMaxYear();

	if (first < min)
		first = min;

	if (last > max)
		last = max;

	for (i = first; i <= last; i++) {
		total += statsGetYearTotal(i);
	}

	return total;
}

int yieldYearCSV(char **ret) {
	return statsYieldYearCSV(ret);
}

int getYearCoAuthorsTotal(int year, int coAuthors) {
	return statsGetYearCoAuthorsTotal(year, coAuthors);
}

int getAuthorsBy(char initial, char** list, int number_displays, int* number_read) {
	if( islower(initial) )
		initial = toupper(initial);

	return getListOfAuthorsByInitial(initial, list, number_displays, number_read);
}

void resetAuthorBy(char initial) {
	if( islower(initial) )
		initial = toupper(initial);

	rewindGeneratorByInitial(initial);
}

void initializeGestauts() {
	initializeAuthorIndex();
	initializeStatistics();
	initializeAuthorCatalog();
}

void leaveGestauts() {
	free(fileread);
	deleteAuthorIndex();
	deleteStatistics();
	deleteAuthorCatalog();
}
