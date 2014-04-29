#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <author.h>
#include "reading.h"

/* COLORS */
#define CLRNRM  "\x1B[0m"
#define CLRRED  "\x1B[1;31m"
#define CLRYEL  "\x1B[33m"
#define CLRBLUE "\x1B[34m"
#define CLRGRN	"\x1B[32m"

#define PRINT_ERROR(msg) 	{ printf(CLRRED "\n\nERROR: " CLRNRM msg "\n\n"); PRESS_ENTER_TO_CONTINUE() }
#define PRINT_INPUT_FIELD(msg) printf("\n"CLRYEL msg ": " CLRNRM);

#define NR_OPTIONS		15
#define NR_FUNCTIONS	15
#define NR_ERRORS		2
#define GREET()				( printf("WELCOME TO GESTAUTS.\n") )
#define BID_FAREWELL()		( printf("BYE BYE.\n") )
#define valid_input(i)  	( ( (i) > -1 ) && ( (i) < NR_FUNCTIONS ) )
#define FLUSH_STDIN() 			\
	while (getchar() != '\n'); 	\

#define PRESS_ENTER_TO_CONTINUE() { 				    						\
	printf(CLRBLUE "\nPRESS" CLRNRM" ENTER " CLRBLUE"TO CONTINUE\n\n" CLRNRM ); \
	if (getchar() != '\n') 														\
		FLUSH_STDIN() 															\
	printf("\n\n\n\n"); }													    \

#define TIME(f) 	 \
	begin = clock(); \
	f				 \
	end = clock();   \

#define PRINT_TIME_SPENT(msg) printf("\n\nTIME SPENT " msg ": %f\n\n", (double)(end - begin) / CLOCKS_PER_SEC);

#define TOTAL_DIGITS(size, n)   \
	size = 0;					\
	do {						\
		size ++;				\
	} while(n /= 10);			\

static int inGestAuts = 1;
static int populated_db = 0;
#ifdef DEBUG
static clock_t begin, end;
#endif

static char* options[NR_OPTIONS] = {
    "EXIT",
	"READ FROM FILE",
	"TOTAL PUBLICATIONS BY YEAR",
	"GET AUTHOR PUBLICATIONS IN YEAR",
	"GET TOTAL SOLO AUTHORS",
	"GET AUTHOR PUBLICATIONS BY YEAR",
	"GET AUTHORS BY INITIAL",
	"TOTAL PUBLICATIONS IN INTERVAL",
	"GET TOP COAUTHORS OF AN AUTHOR",
	"GET PUBLISHED AUTHORS IN INTERVAL",
	"GET YEAR AUTHOR STATS",
	"GET CSV FILE",
	"TOP AUTHORS IN YEAR",
	"AUTHOR YEAR RATIO",
	"PRINT NAME STATISTICS"
};
/* Functions to be replaced with queries */
static void failureprnt() { printf("NON EXISTING FUNCTION\n\n"); }
static void invalid_option_err() { printf("INVALID OPTION\n\n"); }
static void no_file_read_err() { printf("NO FILE WAS READ YET. PLEASE READ FROM A FILE\n\n"); }

/* Controls the end of cmd_interpreter cycle */
static void exitGestAuts() {
	if (populated_db) leaveGestauts();
	inGestAuts = 0;
}

int navigation(int *i, int j, int size) {
	int valid_option, wants_to_read;
	char opt;

	wants_to_read = 1;
	valid_option = 0;

	while(!valid_option) {
		if (j < size)
			printf("\nMORE CONTENT AVAILABLE. CONTINUE READING?");
		else
			printf("\nSHOWING LAST PAGE. LEAVE?");

		printf(" [Y]Yes");

		if (j < size)
			printf(" [N]No");

		if ((*i) > 0)
			printf(" [B]Previous Page");

		putchar(':');

		opt = toupper(getchar());
		FLUSH_STDIN()

		switch(opt) {
			case 'Y':
				if (j < size)
					(*i)++;
				else
					wants_to_read = 0;
				valid_option = 1;
				break;
			case 'N':
				if (j < size) {
					valid_option = 1;
					wants_to_read = 0;
				}
				break;
			case 'B':
				if (*i > 0) {
					valid_option = 1;
					(*i)--;
				}
				break;
		}

		if (!valid_option)
			printf("INVALID OPTION!\n");

		putchar('\n');
	}

	return wants_to_read;
}

void stringNavigation(char** list, int size) {
	int wants_to_read, i, j;

	i = 0;
	j = 0;
	wants_to_read = 1;

	putchar('\n');
	while (wants_to_read) {

		for (j = i * 24; j < size && j < (i + 1) * 24; j++) {
			printf("%s\n",list[j]);
		}

		printf("\x1B[33m\nSHOWING RESULTS\x1B[37m %d \x1B[33m- \x1B[37m%d\x1B[33m OF \x1B[37m%d\n", i * 24 + 1, j, size);

		wants_to_read = navigation(&i, j, size);
	}
}

void tableNavigation(int* list, int first, int size, char *header1, char *header2) {
	int wants_to_read, i, j, digits, z, n;

	i = 0;
	j = 0;
	wants_to_read = 1;

	putchar('\n');
	while (wants_to_read) {

		printf(" ______________ ______________ \n");
		printf("|              |              |\n");
		printf("| %s", header1);
		for (z = 0; z < 14 - (int)strlen(header1) - 1; z++)
			putchar(' ');
		printf("| %s", header2);
		for (z = 0; z < 14 - (int)strlen(header2) - 1; z++)
			putchar(' ');
		printf("|\n");
		printf("|______________|______________|\n");

		for (j = i * 5; j < size && j < (i + 1) * 5; j++) {
			printf("|              |              |\n");
			printf("| %d", first + j);
			n = first + j;
			TOTAL_DIGITS(digits, n)
			for (z = 0; z < 14 - digits - 1; z++) {
				putchar(' ');
			}
			printf("| %d", list[j]);
			n = list[j];
			TOTAL_DIGITS(digits, n)
			for (z = 0; z < 14 - digits - 1; z++) {
				putchar(' ');
			}
			printf("|\n");
			printf("|______________|______________|\n");
		}

		printf("\x1B[33m\nSHOWING RESULTS\x1B[37m %d \x1B[33m- \x1B[37m%d\x1B[33m OF \x1B[37m%d\n", i * 5 + 1, j, size);

		wants_to_read = navigation(&i, j, size);
	}
}

/* Functions reading and printing the required query options */
static void query1() {
	char filename[1024];
	int valid_input = 0;
	int file_reading;

	while ( !valid_input ) {
		PRINT_INPUT_FIELD("ENTER A FILENAME")
		scanf("%s", filename);

		file_reading = read_file(filename);

		if ( file_reading == -1 )
			printf(CLRRED "\n\nERROR: " CLRNRM "FILE DOESN'T EXIST\n\n");

		else {
			if ( file_reading == 1 )
				printf(CLRRED "\n\nERROR: " CLRNRM "%s WAS ALREADY READ\n", filename);

			else if (populated_db) {
				leaveGestauts();
				initializeGestauts();
			}

			valid_input = 1;
		}
	}

	printf("\n"
		CLRYEL"FILEREAD: "
		CLRNRM"%s\n"
		CLRYEL"TOTAL NUMBER OF PUBLICATIONS: "
		CLRNRM"%d\n"
		CLRYEL"TOTAL NUMBER OF AUTHORS: "
		CLRNRM"%d\n"
		CLRYEL"YEAR INTERVAL: "
		CLRNRM"[%d, %d]\n",
		filename, nrPublications(), nrAuthors(), minYear(), maxYear()
		);

	FLUSH_STDIN()

	populated_db = 1;

	PRESS_ENTER_TO_CONTINUE()
}

static void query2() {
	int *totals;
	int min, max;

	totals = getYearsTotal(&min, &max);
	/* Printing a nice table */
	tableNavigation(totals, min, max - min + 1, "YEAR", "PUBLICATIONS");

	printf("\n\n");

	free(totals);
}

static void query3() {
	char author[128];
	char buffer[5];
	int year, total;

	PRINT_INPUT_FIELD("INSERT AUTHOR NAME")
	scanf("%[^\n]s", author);

	FLUSH_STDIN()

	PRINT_INPUT_FIELD("INSERT YEAR")
	scanf("%[^\n]s", buffer);

	FLUSH_STDIN()

	if (isdigit(buffer[0]))
		sscanf(buffer, "%d", &year);
	else {
		PRINT_ERROR("INVALID INPUT")
		return;
	}

	

	total = authorPublicationsInYear(author, year);

	if (total == -1) {
		PRINT_ERROR("NON-EXISTING AUTHOR, PUBLICATIONS OR YEAR");

		return;
	}

	printf(CLRYEL "\n\nTOTAL PUBLICATIONS IN " CLRNRM"%d"CLRYEL":"CLRNRM " %d\n\n", year, total);

	PRESS_ENTER_TO_CONTINUE()

	return;
}

static void query4() {
    int total;

#ifdef DEBUG
    TIME(
	total = totalSoloAuthors();
	)
#else
    total = totalSoloAuthors();
#endif

	printf(CLRYEL "\nTOTAL NUMBER OF SOLO AUTHORS: " CLRNRM"%d\n", total);

#ifdef DEBUG
	PRINT_TIME_SPENT("CALCULATING SOLO AUTHORS")
#endif

	PRESS_ENTER_TO_CONTINUE()
}

static void query5() {
	int size, fst;
	int* yearArray;
	char author_name[1024];

	PRINT_INPUT_FIELD("ENTER A NAME")
	scanf("%1024[^\n]s]", author_name);

	FLUSH_STDIN()

#ifdef DEBUG
	TIME(
	size = getAuthorPublicationsByYear(author_name, &fst, &yearArray);
	)
#else
	size = getAuthorPublicationsByYear(author_name, &fst, &yearArray);
#endif

	if (!size)
		PRINT_ERROR("AUTHOR NOT FOUND")
	else {
		tableNavigation(yearArray, fst, size, "YEAR", "PUBLICATIONS");
		free(yearArray);
	}



#ifdef DEBUG
	PRINT_TIME_SPENT("GETTING YEAR ARRAY")
#endif

}

static void query6() {
	char initial;
	int size;
	AuthorArray author_list;

	PRINT_INPUT_FIELD("ENTER AN INITIAL")
	scanf("\t%c", &initial);
	FLUSH_STDIN()

	author_list = getAuthorsBy(toupper(initial), &size);

	stringNavigation(author_list, size);

	deleteStringBuffer(author_list, size);

}

static void query7() {
	int min, max, total;
	char buffer[5];

	PRINT_INPUT_FIELD("ENTER FIRST YEAR")
	scanf("%[^\n]s", buffer);

	FLUSH_STDIN()

	if (isdigit(buffer[0]))
		sscanf(buffer, "%d", &min);
	else {
		PRINT_ERROR("INVALID INPUT")
		return;
	}

	PRINT_INPUT_FIELD("ENTER LAST YEAR")
	scanf("%[^\n]s", buffer);

	FLUSH_STDIN()

	if (isdigit(buffer[0]))
		sscanf(buffer, "%d", &max);
	else{
		PRINT_ERROR("INVALID INPUT")
		return;
	}

	total = getYearsTotalByInterval(min, max);

	printf(CLRYEL "\nTOTAL PUBLICATIONS BETWEEN YEARS" CLRNRM" %d" CLRYEL " AND" CLRNRM" %d"CLRYEL":"CLRNRM" %d\n\n", min, max, total);

	PRESS_ENTER_TO_CONTINUE()
}

static void query8() {
	char author_name[1024];
	AuthorArray list;
	int list_size, nr_publications;


	PRINT_INPUT_FIELD("ENTER A NAME")
	scanf("%1024[^\n]s]", author_name);

	FLUSH_STDIN()
#ifdef DEBUG
	TIME(
	list = getTopCoauthors(author_name, &list_size, &nr_publications);
	)
#else
	list = getTopCoauthors(author_name, &list_size, &nr_publications);
#endif

	if (!list_size)
		PRINT_ERROR("AUTHOR NOT FOUND")
	else { /* Ensuring correct grammar (plurals and so) */
		stringNavigation(list, list_size);
	}



#ifdef DEBUG
	PRINT_TIME_SPENT("ON QUERY 8")
#endif

	deleteStringBuffer(list, list_size);

}

static void query9() {
	int min, max, size;
	char buffer[5];
	AuthorArray authors;

	PRINT_INPUT_FIELD("ENTER FIRST YEAR")
	scanf("%[^\n]s", buffer);

	FLUSH_STDIN()
	if (isdigit(buffer[0]))
		sscanf(buffer, "%d", &min);
	else {
		PRINT_ERROR("INVALID INPUT")
		return;
	}

	PRINT_INPUT_FIELD("ENTER LAST YEAR")
	scanf("%[^\n]s", buffer);

	FLUSH_STDIN()

	if (isdigit(buffer[0]))
		sscanf(buffer, "%d", &max);
	else {
		PRINT_ERROR("INVALID INPUT")
		return;
	}

#ifdef DEBUG
	TIME(
	size = authorsByInterval(min, max, &authors);
	)
#else
	size = authorsByInterval(min, max, &authors);
#endif

	stringNavigation(authors, size);

#ifdef DEBUG
	PRINT_TIME_SPENT("")
#endif

	deleteStringBuffer(authors, size);
}

/* Get the publications by 1, 2 or 3 authors in a given year and put it in a nice table */
static void query10() {
	int year, totals[3], size, offset, i, j;
	char fileName[50], temp[1024], buffer[5];
	FILE *f;

	size = 0;
	offset = 0;

	PRINT_INPUT_FIELD("ENTER FILE NAME")
	scanf("%[^\n]s", fileName);

	FLUSH_STDIN()
#ifdef DEBUG
	sprintf(fileName + strlen(fileName), ".q10");
#endif
#ifdef DEBUG2
	sprintf(fileName + strlen(fileName), ".q10");
#endif



	PRINT_INPUT_FIELD("ENTER A YEAR")
	scanf("%[^\n]s", buffer);

	FLUSH_STDIN()

	if (isdigit(buffer[0]))
		sscanf(buffer, "%d", &year);
	else {
		PRINT_ERROR("INVALID INPUT")
		return;
	}

	f = fopen(fileName, "a+");

	totals[0] = getYearCoAuthorsTotal(year, 1);
	totals[1] = getYearCoAuthorsTotal(year, 2);
	totals[2] = getYearCoAuthorsTotal(year, 3);

	sprintf(temp,
		" ______________\n"
		"|              |\n"
		"| %d", year);

	TOTAL_DIGITS(size, year);

	offset += 35 + size;

	for (i = 0; i < (14 - size - 1); i++) {
		sprintf(temp + offset + i, " ");
	}
	sprintf(temp + offset + i, "|\n");
	offset += i + 2;

	sprintf(temp + offset, "|______________|______________ \n"
						   "|              |              |\n"
						   "| Authors      | Publications |\n"
		                   "|______________|______________|\n");

	offset += 32 * 4;

	for (j = 0; j < 3; j++) {
		sprintf(temp + offset, "|              |              |\n"
			                   "| %d            | %d", j + 1, totals[j]);

		TOTAL_DIGITS(size, totals[j]);

		offset += 32 + 17 + size;

		for (i = 0; i < (14 - size - 1); i++) {
			sprintf(temp + offset + i, " ");
		}
		sprintf(temp + offset + i, "|\n");
		offset += i + 2;

		sprintf(temp + offset, "|______________|______________|\n" );
		offset += 32;
	}

	printf("%s", temp);
	fprintf(f, "%s", temp);

	fclose(f);

	PRESS_ENTER_TO_CONTINUE()
}

static void query11() {
	int test;
	char *yearCSV, fileName[50];
	FILE *f;

	PRINT_INPUT_FIELD("ENTER FILE NAME")
	scanf("%s", fileName);
	sprintf(fileName + strlen(fileName), ".csv");
	f = fopen(fileName, "a+");
	fprintf(f, "\"Year\",\"#Authors\",\"Publications\"\n");
	for (;;) {
		test = yieldYearCSV(&yearCSV);

		if (!test){
			fprintf(f, "%s", yearCSV);
			free(yearCSV);
		}
		else {
			if (test == 1) {
				fprintf(f, "%s", yearCSV);
				free(yearCSV);
			}

			break;
		}
	}

	printf("\n\n");

	FLUSH_STDIN()

	fclose(f);
}

static void query12() {
	int year, n, count;
	char buffer[5];
	AuthorArray authors;

	PRINT_INPUT_FIELD("ENTER A YEAR")
	scanf("%s", buffer);

	FLUSH_STDIN()

	if (isdigit(buffer[0]))
		sscanf(buffer, "%d", &year);
	else {
		PRINT_ERROR("INVALID INPUT")
		return;
	}

	PRINT_INPUT_FIELD("ENTER NUMBER OF AUTHORS")
	scanf("%s", buffer);
	
	FLUSH_STDIN()

	if (isdigit(buffer[0]))
		sscanf(buffer, "%d", &n);
	else {
		PRINT_ERROR("INVALID INPUT")
		return;
	}
	

#ifdef DEBUG
	TIME(
	count = topAuthorsInYear(year, n, &authors);
	)
#else
	count = topAuthorsInYear(year, n, &authors);
#endif

	if (!count) {
		PRINT_ERROR("INVALID INPUT");
		return;
	}

	stringNavigation(authors, count);

#ifdef DEBUG
	PRINT_TIME_SPENT("CALCULATING TOP AUTHORS")
#endif

	deleteStringBuffer(authors, count);

	return;
}

static void query13() {
	char author[128], buffer[5];
	int year;
	float ratio;

	PRINT_INPUT_FIELD("INSERT AUTHOR NAME")
	scanf("%[^\n]", author);

	FLUSH_STDIN()

	PRINT_INPUT_FIELD("INSERT YEAR")
	scanf("%s", buffer);

	FLUSH_STDIN()
	
	if (isdigit(buffer[0]))
		sscanf(buffer, "%d", &year);
	else {
		PRINT_ERROR("INVALID INPUT")
		return;
	}

	

	ratio = getAuthorYearRatio(author, year);

	if (ratio < 0) {
		PRINT_ERROR("INVALID INPUT");

		return;
	}

	printf(CLRYEL "\n\n%s RATIO IN" CLRNRM " %d"CLRYEL":"CLRNRM" %f%%\n\n", author, year, ratio * 100.0);

	PRESS_ENTER_TO_CONTINUE()

	return;
}

static void query14() {
	char* longest = longestName();
	char* shortest = shortestName();

	printf(
		CLRYEL"\nSHORTEST NAME: "
		CLRGRN"(%d CHARACTERS) "
		CLRNRM" %s\n"
		CLRYEL"LONGEST  NAME: "
		CLRGRN"(%d CHARACTERS) "
		CLRNRM"%s\n"
		CLRYEL"AVERAGE  NAME LENGTH: "
		CLRNRM"%f\n",
		(int)strlen(shortest), shortest, (int)strlen(longest), longest, nameAverage()
		);

	free(longest);
	free(shortest);

	PRESS_ENTER_TO_CONTINUE()
}

/* Function pointer array with the index matching the query number, 0 for exit */
static void(* functions[NR_FUNCTIONS + NR_ERRORS] )() = {
	&exitGestAuts,
	&query1,
	&query2,
	&query3,
	&query4,
	&query5,
	&query6,
	&query7,
	&query8,
	&query9,
	&query10,
	&query11,
	&query12,
	&query13,
	&query14,
    &invalid_option_err,
    &no_file_read_err
};

/* Printing the options array */
static void print_options() {
	int i = 0;

	printf(CLRBLUE "ENTER ONE OF THE FOLLOWING OPTIONS\n" CLRNRM);
	while (i < NR_OPTIONS) {
		printf(CLRYEL "(%d)" CLRNRM " - %s\n", i, options[i]);
		i++;
	}

	putchar('\n');
}

/* Reading an option from user input and setting the index for the funcion array */
static int get_option() {
	int index, read;

	read = scanf("%d", &index);
	FLUSH_STDIN()

	if( !valid_input( index ) || !read)
		index = NR_FUNCTIONS;
	else if ( index > 1 && !populated_db )
		index = NR_FUNCTIONS + 1;

	return index;
}

/* Calling the function and ensuring a PRESS ENTER TO CONTINUE UI */
static void call_option(int index) {
	(*functions[index])();
}

/* Command interpreter kept in cycle until leave option (nr 0) is called */
static void cmd_interpreter() {
	GREET();
/*
#ifdef DEBUG
	call_option(1);
#endif
#ifdef DEBUG2
	call_option(1);
#endif
*/
	while(inGestAuts) {
		print_options();
		call_option( get_option() );
	}

	BID_FAREWELL();
}

void printgestauts() {
printf("\x1b[1;34m	         _              _\n"
       "                | |            | |\n"
       "  __ _  ___  ___| |_ __ _ _   _| |_ ___\n"
       " / _` |/ _ \\/ __| __/ _` | | | | __/ __|\n"
       "| (_| |  __/\\__ \\ || (_| | |_| | |_\\__ \\\n"
       " \\__, |\\___||___/\\__\\__,_|\\__,_|\\__|___/\n"
       "  __/ |\n"
       " |___/\n\n\x1b[0;0m");
}

int main() {
	initializeGestauts();
	printgestauts();
	cmd_interpreter();
	return 0;
}
