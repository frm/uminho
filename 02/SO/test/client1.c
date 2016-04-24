#include "../src/server.h"
#include <unistd.h>
#include <fcntl.h>

int main() {
    int fd = open(SERVER_NAME, O_WRONLY);
    write( fd, "0", sizeof(char) );
    close(fd);
    return 0;
}
