#include "server.h"

// Move this to external includes
#include <sys/types.h>	// Named Pipe
#include <sys/stat.h>	// Named Pipe
#include <unistd.h>		// Open (Read from named pipe)
#include <fcntl.h>		// Open (Read from named pipe)
#include <sys/wait.h>	// wait
#include <stdio.h>		// printf
#include <stdlib.h>		// atoi
#include <strutil.h>
#include "pipe_hash.h"
#include "aggregation.h"

#define NR_HANDLERS			4		// DO NOT move to external .h
#define BUF_SIZE			1024 	// ^ as above
#define TABLE_SIZE			50		// number of districts for the table
#define getDistrict(s)		( strdup( strtok(s, ";") ) )

static PipeTable handl_table;

static void call_child(char *str); // Necessary header definition

static char** parseAggregates(char* agg) {
	int size = 0;
	int max_size = 3;
	char** args = (char**)malloc(sizeof(char*) * max_size);
	char* token = strtok(agg, ":");
	while (token != NULL) {
		args[size++] = strdup(token);
		if (size == max_size) {
			max_size++;
			args = (char**)realloc(args, sizeof(char*) * max_size);
		}
		token = strtok(NULL, ":");
	}

	args[size] = NULL;

	return args;
}

static void deleteAggregatesStr(char** ag) {
	for(int i = 0; ag[i] != NULL; i++)
		free(ag[i]);

	free(ag);
}

static int exit_handl(char* str, Aggregation a)         { return 0; }
static int reload_handl(char* str, Aggregation a)       { return 1; }
static int aggregate_handl(char* str, Aggregation a)    { return 1; }

static int increment_handl(char* str, Aggregation a) {
	int count = atoi( strtok(str, ";") );
	char** agg = parseAggregates( strtok(NULL, ";") );

	updateAggregation(a, agg, count);
	deleteAggregatesStr(agg);

	return 1;
}

static int (* request_handl[NR_HANDLERS])(char* str, Aggregation ag) = {
    &exit_handl,
    &reload_handl,
    &increment_handl,
    &aggregate_handl
};

static void crisis_handl(char* district) {
	char* dup = (char*)malloc(sizeof(district) + 1);
	sprintf(dup, "%s0", district);
	call_child(dup);
	free(dup);
}

static void write_to_child(int fd, char* str, size_t size) {
	char* slice = str_slice(str, size);
	write( fd, slice, sizeof(slice) );
	free(slice);
}

static char* read_from_parent(int fd){
	char buff[1024];
	int i = 0;
	while( read( fd, buff+i, sizeof(char) ) ) i++;
	return strdup(buff);
}

static int dispatch(char *str, Aggregation ag){
	int i = atoi (&str[0]);                    // not sure on this. Maybe char c = str[0]; int i = atoi(&c); ??
	int res = request_handl[i](str+1, ag);
	free(str);
	return res;
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
    int status, pid = -1;
    // write to str.log
    if( !pipe_writer(handl_table, district, fd) ) {
        pid = fork();

        if (pid == 0) {
            int active = 1;
            Aggregation ag = newAggregation(AGGREGATION_SIZE);
            while(active)
                active = dispatch( read_from_parent(fd[1]), ag );
        }
    }

    write_to_child( fd[0], str, strlen(district) );

    waitpid(pid, &status, 0);

    if ( WIFSIGNALED(status)  )
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
	handl_table = newPipeTable(TABLE_SIZE);
	generate_channel();
	receive_request();
	return 0;
}

