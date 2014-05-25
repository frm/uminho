#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

int main() {
    mkfifo("PIPE", 0666);

    int pid1 = fork();
    int pid2;
    int fd = open("PIPE", O_WRONLY);
    int* status;

    if (!pid1) {
        pid2 = fork();

        if (!pid2) {
            char a = 'a';
            for (int i = 0; i < 1024; i++)
                write( fd, &a, sizeof(char) );
            _exit(1);
        }

        else {
            char b = 'b';
            char nd = '\0';
            for (int i = 0; i < 1024; i++)
                write( fd, &b, sizeof(char) );

            wait(status);
            write(fd, &nd, sizeof(char) );
            close(fd);
        }

        _exit(1);
     }

     else {
        int file = open("text.txt", O_CREAT | O_TRUNC | O_WRONLY );
        char buf;

        while( read( fd, &buf, sizeof(char) ) ) {
            write(file, &buf, 1024);
            if(buf == '\0') break;
        }

        close(fd);
        close(file);
        wait(status);
        }

    unlink("PIPE"); // removes the pipe
    return 0;
}