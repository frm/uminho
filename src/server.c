#include "server.h"

// Move this to external includes
#include <sys/types.h>	// Named Pipe
#include <sys/stat.h>	// Named Pipe
#include <unistd.h>		// Open (Read from named pipe)
#include <fcntl.h>		// Open (Read from named pipe)


static int generate_channel() {
	return mkfifo(SERVER_NAME, 0666);
}

static void receive_request() {
	int fd = open(SERVER_NAME, O_RDONLY);
	char buff;

	// Assume user end function creates a semaphore while writing to pipe
	while ( read(pipe, &buff, sizeof(char) ) ) { // when do we stop the cycle?
		/** I'm totally lost here... When do we know the input is over?
		 * Even if we split this into two pipes, when do we mark the input as over?
		 * Also, to avoid semaphore to stop the input completely, split into two pipes just seems a better option
		 */
	}
}

int main() {
	generate_channel();
	receive_request();
	return 0;
}

