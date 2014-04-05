#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "../headers/gestauts.h"

#define NR_OPTIONS		16
#define NR_FUNCTIONS	15
#define NR_ERRORS		2
#define GREET()			( printf("WELCOME TO GESTAUTS.\n") )
#define BID_FAREWELL()	( printf("BYE BYE.\n") )
#define valid_input(i)  ( ( (i) > -1 ) && ( (i) < NR_FUNCTIONS ) )

static int inGestAuts = 1;
static int populated_db = 0;

static char* options[NR_OPTIONS] = {
    "ENTER ONE OF THE FOLLOWING OPTIONS",
    "EXIT",
	"READ FROM FILE",
	"TO BE IMPLEMENTED",
	"TO BE IMPLEMENTED",
	"TO BE IMPLEMENTED",
	"TO BE IMPLEMENTED",
	"GET AUTHORS BY INITIAL",
	"TO BE IMPLEMENTED",
	"TO BE IMPLEMENTED",
	"TO BE IMPLEMENTED",
	"TO BE IMPLEMENTED",
	"TO BE IMPLEMENTED",
	"TO BE IMPLEMENTED",
	"TO BE IMPLEMENTED",
	"PRINT NAME STATISTICS"
};
/* Functions to be replaced with queries */
static void failureprnt() { printf("NON EXISTING FUNCTION\n\n"); }
static void invalid_option_err() { printf("INVALID OPTION\n\n"); }
static void no_file_read_err() { printf("NO FILE WAS READ YET. PLEASE READ FROM A FILE\n\n"); }

static void exitGestAuts() {
	if (populated_db) leaveGestauts();
	inGestAuts = 0;
}

static void query1() {
	char* stats;
	char filename[1024];
	int valid_input = 0;

	while ( !valid_input ) {
		printf("ENTER A FILENAME:\n");
		scanf("%s", filename);

		if ( read_file(filename) == -1 )
			printf("ERROR: FILE DOESN'T EXIST\n\n");
		else
			valid_input = 1;
	}

	stats = getReadStats();
	printf( "%s\n\n", stats );
	free(stats);

	populated_db = 1;
}

static void query6() {
	char* author_list[24];
	char initial, option;
	int i, valid_option, number_read, has_finished;
	int wants_to_read = 1;

	printf("\nENTER AN INITIAL: ");
	scanf("\t%c", &initial);

	while( wants_to_read ) {
		has_finished = getAuthorsBy(initial, author_list, 24, &number_read);

		for(i = 0; i < number_read; i++) {
			printf( "%s\n", author_list[i] );
			free( author_list[i] );
		}

		putchar('\n');

		if ( has_finished )
			wants_to_read = 0;

		else {
			valid_option = 0;

			while (!valid_option) {
				printf("MORE AUTHORS AVAILABLE. CONTINUE READING? [Y/N] ");
				scanf("\t%c", &option);

                option = toupper(option);

				if (option != 'Y' && option != 'N')
					printf("INVALID OPTION\n");

				else {
					if ( option == 'N' )
						wants_to_read = 0;

					valid_option = 1;
				}
			}
		}
	}

	resetAuthorBy(initial);
}

static void query14() {
	char* stats = getAuthorStats();
	printf("%s\n\n", stats );
	free(stats);
}

static void(* functions[NR_FUNCTIONS + NR_ERRORS] )() = {
	&exitGestAuts,
	&query1,
	&failureprnt,
	&failureprnt,
	&failureprnt,
	&failureprnt,
	&query6,
	&failureprnt,
	&failureprnt,
	&failureprnt,
	&failureprnt,
	&failureprnt,
	&failureprnt,
	&failureprnt,
	&query14,
    &invalid_option_err,
    &no_file_read_err
};

static void print_options() {
	int i = 1;

	printf("%s\n", options[0]);
	while (i < NR_OPTIONS)
		printf("\x1B[33m(%d)\x1B[37m - %s\n", i - 1, options[i++]);

	putchar('\n');
}

static int get_option() {
	int index;

	scanf("%d%*[^\n]s", &index);

	/* Error handling */
	if( !valid_input( index ) )
		index = NR_FUNCTIONS;
	else if ( index > 1 && !populated_db )
		index = NR_FUNCTIONS + 1;

	return index;
}

static void call_option(int index) { (*functions[index])(); }

static void cmd_interpreter() {
	GREET();

	while(inGestAuts) {
		print_options();
		call_option( get_option() );
	}

	BID_FAREWELL();
}

int main() {
	initializeGestauts();
	cmd_interpreter();
	return 0;
}

