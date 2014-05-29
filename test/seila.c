#include <sys/types.h>	// Named Pipe
#include <sys/stat.h>	// Named Pipe
#include <unistd.h>		// Open (Read from named pipe)
#include <fcntl.h>		// Open (Read from named pipe)
#include <sys/wait.h>	// wait
#include <stdio.h>		// printf
#include <stdlib.h>		// atoi

int main(void) {
    int pid1 = fork();
    int status;
    
    if (!pid1) {
        printf( "FIRST CHILD. PID: %d\nI WILL BE REAPED\n", getpid() );
		pause();
	}
    
    else {
    
        int pid2 = fork();
    
        if (!pid2) {
            printf( "SECOND CHILD. PID: %d\nI WILL TERMINATE MYSELF\n", getpid() );
            _exit(0);
        }

        else {
    
            int status;
            waitpid(pid2, &status, 0);

            if ( WIFSIGNALED(status) ) printf("CHILD %d WAS REAPED\n", pid2);
            else printf("CHILD %d TERMINATED ITSELF\n", pid2);
        }
        
        waitpid(pid1, &status, 0);

        if ( WIFSIGNALED(status) ) printf("CHILD %d WAS REAPED\n", pid1);
        else printf("CHILD %d TERMINATED ITSELF\n", pid1);
    }

	printf("\n\n### IGNORE THIS. I'M STILL RUNNING ###\n\n");

    return 0;
}

