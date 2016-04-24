
#include <unistd.h>   /* chamadas ao sistema: defs e decls essenciais */
#include <fcntl.h>    /* O_RDONLY, O_WRONLY, O_CREAT, O_* */


int     open(const char *path, int oflag [, mode]);

ssize_t read(int fildes, void *buf, size_t nbyte);

ssize_t write(int fildes, const void *buf, size_t nbyte);

int     close(int fildes);


_____________________

#include <unistd.h> /* chamadas ao sistema: defs e decls essenciais */
#include <sys/wait.h> /* chamadas wait*() e macros relacionadas */



pid_t getpid(void);

pid_t getppid(void);

pid_t fork(void);

void _exit(int status);

pid_t wait(int *status);

pid_t waitpid(pid_t pid, int *status, int options);

int WIFEXITED(int status); /* macro */

int WEXITSTATUS(int status); /* macro */

_____________________

#include <unistd.h>     /* chamadas ao sistema: defs e decls essenciais */

int execl(const char *path, const char *arg0, ..., NULL);

int execlp(const char *file, const char *arg0, ..., NULL);

int execv(const char *path, char *const argv[]);

int execvp(const char *file, char *const argv[]);

_____________________


#include <unistd.h>     /* chamadas ao sistema: defs e decls essenciais */

int dup(int fd);

int dup2(int fd1, int fd2);

_____________________


#include <unistd.h>     /* chamadas ao sistema: defs e decls essenciais */

int pipe(pd[2]);


_____________________


#include <sys/types.h>
#include <sys/stat.h>

int mkfifo(const char *pathname, mode_t mode);

_____________________


#include <signal.h>
#include <sys/types.h>

typedef void (*sighandler_t)(int);

sighandler_t signal(int signum, sighandler_t handler);

int kill(pid_t pid, int sig);

unsigned int alarm(unsigned int seconds);

int pause(void);




