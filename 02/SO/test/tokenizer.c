#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char** parseAggregates(char* agg) {
	int size = 0;
	int max_size = 3;
	char** args = (char**)malloc(sizeof(char*) * max_size);
	char* token = strtok(agg, ":");


	while (token != NULL) {
		printf("%s\n", token);
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

void deleteAggregatesStr(char** ag) {
	for(int i = 0; ag[i] != NULL; i++)
		free(ag[i]);

	free(ag);
}

void aggregate(char** agg, int level, char* file) {
	printf("\nShould have aggregated: ");
	for (int i = 0; agg[i] != NULL; i++)
		printf("%s:", agg[i]);

	printf("NULL\n");

	printf("Up to level %d\n", level);

	printf("Into file: %s", file);
}

void increment(char** agg, int inc) {

	printf("\nShould have incremented: ");
	for (int i = 0; agg[i] != NULL; i++)
		printf("%s:", agg[i]);

	printf("NULL\n");

	printf("Up to %d\n", inc);
}


void aggregate_parser(char* str) {
	printf("### IT'S AGGREGATE ###\n");

	int level = atoi( strtok(str, ";") );

	char* file = strtok(NULL, ";");


	char* token = strtok(NULL, ";");
	char** agg = parseAggregates(token);

	aggregate(agg, level, file);

	deleteAggregatesStr(agg);
}

void increment_parser(char* str) {
	printf("### IT'S INCREMENT ###\n");

	char* token = strtok(str, ";");

	int level = atoi(token);

	token = strtok(NULL, ";");

	char** agg = parseAggregates(token);

	increment(agg, level);

	deleteAggregatesStr(agg);

}


//"0Braga:Braga:Dume;10;path/to/file"
int main(void) {
	char buf[1024];
	while( scanf("%s", buf) ) {

		if (buf[0] == '0')
			increment_parser(buf + 1);
		else
			aggregate_parser(buf + 1);

		while ( getchar() != '\n' );
	}

	return 0;
}
