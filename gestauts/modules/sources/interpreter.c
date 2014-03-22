#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
static void successprnt() { printf("SUCCESS\n"); }
static void failureprnt() { printf("FAILURE\n"); }
static void invalid_option_err() { printf("INVALID OPTION\n"); }

static void(* functions[NR_FUNCTIONS + NR_ERRORS] )() = {
	&leaveGestAuts,
	&successprnt,
	&failureprnt,
	&failureprnt,
	&failureprnt,
	&failureprnt,
	&failureprnt,
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

int main() {
	cmd_interpreter();
	return 0;
}

