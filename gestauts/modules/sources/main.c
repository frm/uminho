#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gestauts.h"

#define NR_OPTIONS		13
#define NR_FUNCTIONS	14
#define NR_ERRORS		1
#define GREET()			( printf("WELCOME TO GESTAUTS.\n") )
#define BID_FAREWELL()	( printf("BYE BYE.\n") )
#define valid_input(i)  ( ( (i) > -1 ) && ( (i) < NR_OPTIONS ) )

static int inGestAuts = 1;
static char* options[NR_OPTIONS] = {
	"ENTER ONE OF THE FOLLOWING OPTIONS",
	"READ FROM FILE",
	"TO BE IMPLEMENTED",
	"TO BE IMPLEMENTED",
	"TO BE IMPLEMENTED",
	"TO BE IMPLEMENTED",
	"TO BE IMPLEMENTED",
	"TO BE IMPLEMENTED",
	"TO BE IMPLEMENTED",
	"TO BE IMPLEMENTED",
	"TO BE IMPLEMENTED",
	"TO BE IMPLEMENTED",
	"TO BE IMPLEMENTED"
};
/* Functions to be replaced with queries */
static void leaveGestAuts() { inGestAuts = 0; }
static void failureprnt() { printf("NON EXISTING FUNCTION\n"); }
static void invalid_option_err() { printf("INVALID OPTION\n"); }

static void query1() {
	char* stats;
	char filename[1024];
	int valid_input = 0;

	while ( !valid_input ) {
		printf("ENTER A FILENAME:\n");
		scanf("%1024s\n", filename);

		if ( read_file(filename) == -1 )
			printf("ERROR: FILE DOESN'T EXIST\n\n");
		else
			valid_input = 1;
	}

	stats = getReadStats();
	printf( "%s\n", stats );
	free(stats);
}

static void query6() {
	printf("\n");
}
static void(* functions[NR_FUNCTIONS + NR_ERRORS] )() = {
	&leaveGestAuts,
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
    &invalid_option_err
};

static void print_options() {
	int i = 1;

	printf("%s\n", options[0]);
	while (i < NR_OPTIONS)
		printf("\x1B[33m(%d)\x1B[37m - %s\n", i, options[i++]);
}

static int get_option() {
	int index;

	scanf("%d%*[^\n]s", &index);

	/* Error handling */
	if( !valid_input( index ) )
		index = NR_OPTIONS + 1;

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

static void init() {
	initialize_author_index();
}

int main() {
	init();
	cmd_interpreter();
	return 0;
}

