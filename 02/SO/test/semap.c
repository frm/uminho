/* ## HEADS UP ##
 * Since semaphores are a complex thing and very important to the project,
 * I'm writing this fully commented file with explicit behaviour
 * so we can consult this anytime we want and get info about semaphores without going through 123854902231 websites
 * And read stuff over and over until we understand it
 * See:
 * http://brickos.sourceforge.net/docs/APIs/html-c++/semaphore_8h.html
 * http://www.amparo.net/ce155/sem-ex.html
 */


// Compile with CC -Wall -Wextra -pedantic -lpthread sema.c

#include <semaphore.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/wait.h>
#include <sys/mman.h>


void mutex_write() {
	int fd = open("double_write.txt", O_CREAT | O_WRONLY | O_TRUNC);
	sem_t* mutex = (sem_t*) mmap(0, sizeof(sem_t), PROT_READ|PROT_WRITE, MAP_SHARED|MAP_ANONYMOUS, -1, 0);
	/** See below, on pshared comment.
	  * The memory needs to be mapped to a shared mem address
	  * mmap function does that. Brief explanation (based off man pages):
	  * pa=mmap(addr, len, prot, flags, fildes, off);
	  * addr => tl;dr... Seriously, someone fill this one in
	  * pa => address of the mapped memory region
	  * len => number of bytes to be mapped
	  * fildes => mmap maps len bytes to the object represented by fildes.
	  * 		## WARNING. Not sure of what I write in the next 3 lines. It was what I understood from the man pages ##
	  *				Since we're using it inside the same process, no need to assign it to a file descriptor
	  *				If we wanted to make it available outside the process, we could create a file descriptor
	  *				assign it to fildes and then any process that refers to fildes would be getting the mapped memmory address
	  * off => offset for bytes. Didn't really understand this one. Someone please check the man pages and fill me in
	  * prot => permissions for mapped memory area. Can be:
	  *			PROT_READ  Data can be read.
      *         PROT_WRITE Data can be written.
      *         PROT_EXEC  Data can be executed.
      *         PROT_NONE  Data cannot be accessed.
      *			or a combination of above, using xor ( | )
      * flags => flags (lol) on handling memory area:
      *				MAP_SHARED   Changes are shared.
      *				MAP_PRIVATE  Changes are private.
      *				MAP_FIXED    Interpret addr exactly. (I believe this translate to NO CHANGES!!1!)
	  *				MAP_ANONYMOUS The mapping is not backed by any file; its contents are initialized to zero.
	  */

	int status;

	int pshared = 1;
	/* pshared = 0  => semaphore is shared between the threads of a single process.
	 * For uses like a global variable or heap allocated.
 	 * pshared != 0 => semaphore is shared between processes. It needs to be located in a shared memory region.
 	 * This means it is acessible to forked child. In fact, any process that can has access to the shared memory region can use the semaphore
	 */

	int num_keys = 1; // Setting this to 1, means only one process can access the semaphore

	sem_init(mutex, pshared, num_keys); // Note I only used variables to explain them. normal I'd sem_init(&mutex, 0, 1)

	int pid = fork();

	if (! pid) {
		char c = 'b';
		sem_wait(mutex); // Tries to access the semaphore, decrementing the number of keys (blocks if num_keys == 0)

		for (int i = 0; i < 10000; i++)
			write ( fd, &c, sizeof(char) );

		sem_post(mutex);					// increments the number of keys on semaphore
		_exit(1);
	}

	else {
		char c = 'a';
		sem_wait(mutex);

		for (int i = 0; i < 10000; i++)
			write ( fd, &c, sizeof(char) );

		sem_post(mutex);
		wait(&status);
		sem_destroy(mutex);					// destroy the semaphore after using it

	}

}



int main(void) {
	mutex_write();
	return 0;
}

