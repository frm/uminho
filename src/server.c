#include "server.h"

// Move this to external includes
#include <sys/types.h>	// Named Pipe
#include <sys/stat.h>	// Named Pipe
#include <unistd.h>		// Open (Read from named pipe)
#include <fcntl.h>		// Open (Read from named pipe)
#include <sys/wait.h>	// wait
#include <stdio.h>		// printf
#include <stdlib.h>		// atoi

#define NR_HANDLERS			2		// DO NOT move to external .h
#define BUF_SIZE			1024 	// ^ as above
#define TABLE_SIZE			50		// number of districts for the table
#define getDistrict(s)		( strdup( strtok(s, ";") ) )

static PipeTable handl_table;

/*
static void increment_handl(char *str) {
	incrementar([Braga, Braga, Dume, NULL], 2);
	Braga;02;Braga:Dume
	strtok(";")
	hash[token] => str + strlen(token);
}

static void aggregate_handl(char *str) {
	printf("YET TO BE IMPLEMENTED\n");
}

static void (* request_handl[NR_HANDLERS])(char *str) = {
	&increment_handl,
	&aggregate_handl
};*/

static void crisis_handl(char* district) {
	char* dup = (char*)malloc(sizeof(district) + 1);
	sprintf(dup, "%s0", district);
	call_child(dup);
	free(dup);
}

static void write_to_child(int fd, char* str, size_t size) {
	char* slice = str_slice( str, size );
	write(fd[0], slice, sizeof(slice));
	free(slice);
}

/* child needs to parse string
 * The string format is <district>;<indicator><value>;<aggregation>;<filename if needed>
 * <indicator> can be 0, 1 or 2
 * if 0 => read from log
 * if 1 => increment
 * if 2 => aggregate
 */
static void call_child(char *str) {
	char* district = getDistrict(str);
	int fd[2];
	int pid, status;
	// write to str.log
	if( !pipe_writer(handl_table, district, fd) ) {
		pid = fork();

		if (pid == 0) {
			Aggregation ag = newAggregation(AGGREGATION_SIZE);
			//do something
		}
	}
	
	write_to_child( fd[0], str, strlen(district) );

	waitpid(pid, &status, 0); // double check that 0
	
	if ( WIFEXIT(status) == SIGCHILD )  // this isn't right, change that
		crisis_handl(district);

	free(district);
}

static int generate_channel() {
	return mkfifo(SERVER_NAME, 0666);
}

static void receive_request() {
	int fd = open(SERVER_NAME, O_RDONLY);
	char buff[1024];
	int active = 1;

	while (active) { 
		int i = 0;
		while( read( fd, buff+i, sizeof(char) ) ) i++;
		call_child(buff);
	}
}

int main() {
	// NOTE: Create signal to save data on SIGQUIT
	handl_table = newPipeTable(TABLE_SIZE;
	generate_channel();
	receive_request();
	return 0;
}

