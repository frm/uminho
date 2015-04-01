#ifndef _HASH_H
#define _HASH_H

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include "strutil.h"

typedef struct hash *hash;

hash new_hash(int size);

int add_to_hash(hash* h, char* args[], int size);

int get_top(hash* h, char** ret);

int update_by_level(hash* h, char* args[], int level);
int update_at_top(hash* h, char* key);

void reset_level(hash* h, int level);

#ifdef DEBUG
void print_hash(hash h, int level);
#endif

#endif
