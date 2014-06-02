#define _POSIX_SOURCE
#include "server.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <strutil.h>
#include <signal.h>

#include "pipe_hash.h"
#include "aggregation.h"

#define NR_HANDLERS			4
#define BUF_SIZE			1024
#define TABLE_SIZE			50		// number of districts for the table
#define set_children_signals()          \       /** Sets the children signal handles */
            signal(SIGINT,  SIG_IGN);   \
            signal(SIGQUIT, SIG_DFL);   \
            signal(SIGUSR1, SIG_DFL);   \
            signal(SIGUSR2, SIG_DFL);   \
            signal(SIGTERM, SIG_DFL);   \
            signal(SIGKILL, SIG_DFL);   \
            signal(SIGCHLD, SIG_IGN)    \

#define set_parent_signals()                 \  /** Sets the parent signal handls */
            signal(SIGINT,  clear_struct);   \
            signal(SIGQUIT, clear_struct);   \
            signal(SIGUSR1, clear_struct);   \
            signal(SIGUSR2, clear_struct);   \
            signal(SIGTERM, clear_struct);   \
            signal(SIGKILL, clear_struct);   \
            signal(SIGCHLD, revive)          \

#define init_server()                                   \       /** Starts the server */
            set_parent_signals();                       \
            handl_table = newPipeTable(TABLE_SIZE);     \
            is_active = 1;                              \
            mkfifo(SERVER_NAME, 0666)                   \

static PipeTable handl_table;   /** PipeTable that contains the descriptors, district names and pids for child communication */
static int is_active;           /** Control variable that enables FIFO reading */

static void call_child(char *str); // Necessary header definition

/** Returns the district from the pipe args string */
static char* get_district(char* str) {
    char* str1 = str_dup(str);
    char* str2 = str_dup ( strtok(str1, ";") );
    free(str1);
    return str2;
}

/** Clears everything up for server exit */
void clear_struct(int s) {
    is_active = 0;
    shutdown_children(handl_table);
    deletePipeTable(handl_table);
    unlink(SERVER_NAME);
}

/** Parses string generating array of aggregates for increment or collect */
static char** parseAggregates(char* agg, char* name) {
	int max_size = 3;
	char** args = (char**)malloc(sizeof(char*) * max_size);
    int size = 1;

    args[0] = str_dup(name);

    char* token = strtok(agg, ":");

	while (token != NULL) {
		args[size++] = strtrim(token);
		if (size == max_size) {
			max_size++;
			args = (char**)realloc(args, sizeof(char*) * max_size);
		}
		token = strtok(NULL, ":");
	}

	args[size] = NULL;

	return args;
}

/** Deletes aggregates string generated */
static void deleteAggregatesStr(char** ag) {
	for(int i = 0; ag[i] != NULL; i++)
		free(ag[i]);

	free(ag);
}

/** Writes the increment string argument to a .log with name of the district */
static void write_to_log(char *district, char *agg){
	if(agg[0] == '2') {
		char logfile[1024];
		sprintf(logfile, "%s.log", district);
        int size = strlen(agg);
        char* msg = (char*)malloc(size);
        sprintf(msg, "%s\n", agg + 1);

		int fd = open(logfile, O_CREAT | O_WRONLY | O_APPEND, 0666);
		write(fd, msg, size);
    	close(fd);

        free(msg);
    }
}

/** Receives a string and parses it, generating valid input for increment function. It then applies it to given aggregate */
static int increment_handl(char* str, Aggregate a) {
    int count = atoi( strtok(str, ";") );
    char** agg = parseAggregates( strtok(NULL, ";"), getAggregateName(a) );
    incrementAggregate(a, agg, count);
    deleteAggregatesStr(agg);
    return 1;
}

/** Returns 0 for dispatch, causing the child to end */
static int exit_handl(char* str, Aggregate a)         { return 0; }

/** Reads from the log of a given district, passing the read arguments to re-increment the aggregate */
static int reload_handl(char* str, Aggregate a) {
    char logfile[1024];
    sprintf(logfile, "%s.log", str);
    char buf[1024];
    int i = 0;
    int fd = open(logfile, O_RDONLY);

    while (read(fd, buf + i, sizeof(char)) > 0) {
        if (buf[i] == '\n') {
            increment_handl(buf, a);
            i = 0;
        }
        else i++;
    }

    return 1;
}

/** Receives a string and parses it, generating valid input for collect function. It then applies it to given aggregate */
static int aggregate_handl(char* str, Aggregate a)    {
	int level = atoi( strtok(str, ";") );

    int pid = atoi( strtok(NULL, ";") );

	char *filepath = str_dup( strtok(NULL, ";"));

	char** agg = parseAggregates( strtok(NULL, ";"), getAggregateName(a) );

	if ( collectAggregate(a, agg, level, filepath) < 0 )
        kill(pid, SIGQUIT);
    else
        kill(pid, SIGINT);

    deleteAggregatesStr(agg);

	return 1;
}

/** Array of function pointers for child function handling */
static int (* request_handl[NR_HANDLERS])(char* str, Aggregate a) = {
    &exit_handl,
    &reload_handl,
    &increment_handl,
    &aggregate_handl
};

static void revive (int s); // Necessary header declaration

/** Prepares the input for a child to reload itself */
static void crisis_handl(char* district) {
	char* dup = (char*)malloc( 2 * strlen(district) + 3);
	sprintf(dup, "%s;1%s", district, district);
	call_child(dup);
	free(dup);
}

/** Parses a string to call the correct function from the function pointer arrays */
static int dispatch(char *str, Aggregate a){
	char c = str[0];
    int i = atoi(&c);
	int res = request_handl[i](str+1, a);
	return res;
}

/** Rearms the SIGCHLD, collecting the child pid and closing its descriptors.
  * It then proceeds to remove the dead child from the pipe table and calling call_child with arguments
  * that allows it to reload the child
  */
static void revive (int s) {
    signal(SIGCHLD, revive);
    int status;
    int chld_pid = wait(&status);
    if ( WIFSIGNALED(status) ) {
        char* name = bury_dead_child(handl_table, chld_pid);
        crisis_handl(name);
        free(name);
    }
}

/** Child stays active reading from parent and dispatching requests */
static void read_from_parent(int fd, char* district) {
    Aggregate a = newAggregateFull(district, 0);
    int active = 1;
    while(active) {
        int i = 0;
        char buff[1024];

        while ( read( fd, buff + i, sizeof(char) ) > 0 && buff[i] != '\0' ) i++;

        active = dispatch(buff, a);
    }

    deleteAggregate(a);
    close(fd);

}

/* Parses a string and calls the correct function handling, creating the child if needed */
static void call_child(char *str) {
    char* district = get_district(str);
    char* slice = str_slice(str, strlen(district) + 1);
    int* fd = (int*)malloc(sizeof(int) * 2);
    int pid = -1;

    write_to_log(district, slice);

    if( !pipe_writer(handl_table, district, &fd) ) {

        pid = fork();

        if (pid == 0) {
            set_children_signals();
        	close(fd[1]);
            read_from_parent(fd[0], district);
            exit(EXIT_SUCCESS);
        }

	   else {
            close(fd[0]);
            set_pid(handl_table, district, pid);
        }
    }

    write(fd[1], slice, strlen(slice) + 1);
    free(slice);
    free(fd);
    free(district);
}

/** Reads from a named pipe, dispatching requests once they arrive */
static void receive_request() {
	int fd;
	char buff[1024];

    while (is_active) {
		fd = open(SERVER_NAME, O_RDONLY);
		int i = 0;
        while ( read(fd, buff + i, 1) > 0 ) {
            if (buff[i] == '\0') {
                call_child(buff);
                i = 0;
            }
            else i++;
        }
		close(fd);
    }

}

int main() {
    init_server();
	receive_request();
	return 0;
}

