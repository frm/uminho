#include "server.h"

// Move this to external includes
#include <sys/types.h>	// Named Pipe
#include <sys/stat.h>	// Named Pipe
#include <unistd.h>		// Open (Read from named pipe)
#include <fcntl.h>		// Open (Read from named pipe)
#include <sys/wait.h>	// wait
#include <stdio.h>		// printf
#include <stdlib.h>		// atoi

#define NR_HANDLERS		2		// DO NOT move to external .h
#define BUF_SIZE		1024 	// ^ as above

static void increment_handl() {
	if ( fork() == 0 ) {
		// Insert function to read input
		int pid = fork();

		if (!pid) {
			printf("DOUBLE CHILD CREATED FOR INCREMENT\n");
			_exit(0);
		}
		
		else {
			int status;
			waitpid(pid, &status, 0);

			if ( WIFEXITED(status) != 0 )
				printf("I SHOULD'VE RESTARTED MY CHILD");
			/* Move this fork to another function to allow for recursive implementation */
		}
	}
}

static void aggregate_handl() {
	printf("YET TO BE IMPLEMENTED\n");
}

static void (* request_handl[NR_HANDLERS])() = {
	&increment_handl,
	&aggregate_handl
};

static void dispatch(int i) {
	if ( i < 0 || i >= NR_HANDLERS)
		perror("INVALID OPTION");
	else
		request_handl[i]();
}

static int generate_channel() {
	return mkfifo(SERVER_NAME, 0666);
}

static void receive_request() {
	int fd = open(SERVER_NAME, O_RDONLY);
	char buff;

	while (1) { 
		read( fd, &buff, sizeof(char) );
		dispatch( atoi(&buff) );
	}
}

int main() {
	// NOTE: Create signal to save data on SIGQUIT
	generate_channel();
	receive_request();
	return 0;
}

