#include "server.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/wait.h>
#include <stdio.h>
#include <stdlib.h>
#include <strutil.h>
#include "pipe_hash.h"
#include "aggregation.h"

#define NR_HANDLERS			4		// DO NOT move to external .h
#define BUF_SIZE			1024 	// ^ as above
#define TABLE_SIZE			50		// number of districts for the table

static PipeTable handl_table;

static void call_child(char *str); // Necessary header definition


static char* get_district(char* str) {
    char* str1 = strdup(str);
    char* str2 = strdup ( strtok(str1, ";") );
    free(str1);
    return str2;
}

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

static void write_to_log(char *district, char *agg){
	if(agg[0] == '2'){
		char logfile[1024];
		sprintf(logfile, "%s.log", district);
		int fd = open(logfile, O_CREAT | O_WRONLY | O_APPEND, 0666);

		write( fd, agg + 1, strlen(agg) );
        write( fd, "\n", sizeof(char) );

    	close(fd);
    }
}

static int exit_handl(char* str, Aggregation a)         { return 0; }
static int reload_handl(char* str, Aggregation a)       { return 1; }

static int aggregate_handl(char* str, Aggregation a)    {
	int level = atoi( strtok(str, ";") );
	char *filepath = strdup( strtok(NULL, ";"));

	char** agg = parseAggregates( strtok(NULL, ";") );

	collectAggregate(a, agg, level, filepath);
    printf("###\n AGGREGATED\n###\n");

    deleteAggregatesStr(agg);

	return 1;
}


static int increment_handl(char* str, Aggregation a) {
	int count = atoi( strtok(str, ";") );
	char** agg = parseAggregates( strtok(NULL, ";") );

    updateAggregation(a, agg, count);

    printf("###\n INCREMENTED\n###\n");
	printAggregation(a);

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
	char* dup = (char*)malloc(strlen(district) + 3);
	sprintf(dup, "%s;0", district);
	call_child(dup);
	free(dup);
}

static char* read_from_parent(int fd) {
	char buff[1024];
	int i = 0;
	while( read( fd, buff+i, sizeof(char) ) > 0 ) i++;
	return strdup(buff);
}

static int dispatch(char *str, Aggregation ag){
	char c = str[0];
    int i = atoi(&c);
	int res = request_handl[i](str+1, ag);
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
    //printf("### ARGS %s###\n", str);
    char* district = get_district(str);
    char* slice = str_slice(str, strlen(district) + 1);
    int* fd = (int*)malloc(sizeof(int) * 2);
    int status, pid = -1;
    write_to_log(district, slice);
    if( !pipe_writer(handl_table, district, &fd) ) {
        pid = fork();

        if (pid == 0) {
        	close(fd[1]);

            int active = 1;
            Aggregation ag = newAggregation(AGGREGATION_SIZE);
            while(active) {
                int i = 0;
                char buff[1024];
                while ( read( fd[0], buff + i, sizeof(char) ) > 0 && buff[i] != '\0' ) i++;

                active = dispatch(buff, ag);
            }
            deleteAggregation(ag);
            close(fd[0]);
            exit(EXIT_SUCCESS);
        }

	   else {
            close(fd[0]);
            set_pid(handl_table, district, pid);
        }
    }

    write( fd[1], slice, strlen(slice) + 1 );
    free(slice);
    free(fd);
    waitpid(pid, &status, WNOHANG);

    if ( WIFSIGNALED(status)  )
        crisis_handl(district);
    else
        close(fd[1]);

    free(district);
}

static int generate_channel() {
	return mkfifo(SERVER_NAME, 0666);
}

static void receive_request() {
	int fd;
	char buff[1024];
	int active = 1;

    while (active) {
		fd = open(SERVER_NAME, O_RDONLY);
		int i = 0;
        while ( read(fd, buff + i, 1) > 0 ) {
            if (buff[i] == '\0') {
                printf(" I STOPPED BECAUSE OF NULL\n");
                printf("### GONNA CALL %s\n\n", buff);
                call_child(buff);
                i = 0;
            }
            else i++;
        }
		close(fd);
    }

}

int main() {
	handl_table = newPipeTable(TABLE_SIZE);
	generate_channel();
	receive_request();
    deletePipeTable(handl_table);
	return 0;
}

