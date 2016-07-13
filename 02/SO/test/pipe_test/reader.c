#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>


int main()
{
    int fd;
    
    char buf[1024];

    /* open, read, and display the message from the FIFO */
    fd = open("myfifo", O_RDONLY);
    read(fd, buf, 1024);
    printf("%s\n", buf);
    close(fd);

    return 0;
}


/*
int main(){
	char buffer[1024];
	int fd = open("pipe", O_WRONLY);

	read(fd, buffer, 1024);
    printf("Received: %s\n", buffer);              // write 128b to PIPE
   	_exit(1);
   	close(fd);
   	return 0;

}
*/
