#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>

/*
int main(){

	mkfifo("pipe", O_WRONLY);
	int fd = open("pipe", O_WRONLY);
	char* str128 = "look at my horse my horse is amazing, give it a lick, it tastes just like raisins, have a touch at it's mane and then he turns b";
    write( fd, str128, sizeof(str128) );              // write 128b to PIPE
   	_exit(1);
   	close(fd);
   	unlink("pipe");

   	return 0;

}*/

   	int main()
{
    int fd;
    char * myfifo = "myfifo";

    /* create the FIFO (named pipe) */
    mkfifo(myfifo, 0666);
	  char* str128 = "wolololololololololololololololololololololololololololololololololololololololololololololololololololololololololololololololowolololololololololololololololololololololololololololololololololololololololololololololololololololololololololololololololo";
    
    /* write "Hi" to the FIFO */
    fd = open(myfifo, O_WRONLY);
    printf("%lu\n", sizeof(str128));
    write(fd, str128, 256);
    close(fd);

    /* remove the FIFO */
    unlink(myfifo);

    return 0;
}