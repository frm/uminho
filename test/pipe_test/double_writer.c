#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <string.h>

int main() {
    mkfifo("PIPE", 0666);

    int pid2;
    int fd = open("PIPE", O_WRONLY); // Open pipe
    int* status; // placeholder
    int file = open("./text.txt", O_CREAT | O_TRUNC | O_WRONLY );    // Create a file for printing the pipe output
    int pid1 = fork(); // Creates first pipe for writing to file

    if (!pid1) {

        pid2 = fork(); // Creates second pipe for concurrent writing to PIPE

        if (!pid2) {
            char* str128 = "I HAVE TO WRITE 100 CHARS HERE SO LET ME SEE... WAIT, BETTER MAKE IT 128. MAN, THIS FEELS LIKE TWITTER BUT THE OPPOSITE. ABCDEF";
            write( file, str128, sizeof(str128) );              // write 128b to PIPE
            _exit(1);                                         // Dude, kill yourself
        }

        else {
            char* mystr = "THIS STRING IS NOT LIKE THE OTHER ONE. IT IS MUCH BETTER. AND IT IS GETTING BETTER ALL THE TIME SHUT UP RINGO NO ONE LIKES YOU.";
            write( file, mystr, sizeof(mystr) );              // write 128b to PIPE

            wait(status);                               // wait for your kid
            close(fd);                                  // Shut your pipe
        }

        _exit(1);                                       // And now it's time to die
     }

     else {
        char buf[1024];
        int file = open("./text.txt", O_CREAT | O_TRUNC | O_WRONLY );    // Create a file for printing the pipe output

        read(fd, buf, 1024);
        write( file, buf, strlen(buf) );

        close(fd);
        close(file);
        wait(status);
        }

    unlink("PIPE"); // removes the pipe
    return 0;
}