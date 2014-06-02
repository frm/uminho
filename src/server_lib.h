#ifndef SERVER_LIB_H
#define SERVER_LIB_H

/*Main function: Incrementar; Increments the given value to the location given in prefix*/
int incrementar(char* prefix[], int value);

/**Main function: Agregar; Writes info about the given arguments into a file*/
int agregar(char *prefix[], unsigned level, char *path);
#endif
