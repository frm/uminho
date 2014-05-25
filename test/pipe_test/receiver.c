#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

int main() {
    int fd = open("PIPE", O_WRONLY);
    int file = open("a.txt", O_CREAT | O_TRUNC | O_WRONLY );
    char buf[1025];

    for (int i = 0; i < 2; i++) {
        read(fd, buf, 1024);
        write(file, buf, 1024);
    }

    close(fd);
    close(file);

    return 0;
}