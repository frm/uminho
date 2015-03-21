#include "hash.h"

typedef struct bucket_node {
    char* key;
    struct bucket_node* next;
    hash subnodes;
} bucket_node, *bucket;

struct hash {
    int size;
    int n_elements;
    bucket *table;
};


static void delete_bucket(bucket b);

void delete_hash(hash h) {
    if(!h)
        return;

    for(int i = 0; i < h -> n_elements; i++)
        delete_bucket(h -> table[i]);

    free(h);
}

static void delete_bucket(bucket b) {
    if(!b)
        return;
    delete_hash(b -> subnodes);
    delete_bucket(b -> next);
    free(b -> key);
    free(b);
}

static bucket new_bucket(char* key) {
    bucket b = (bucket)malloc(sizeof(bucket_node));
    b -> key = strdup(key);
    b -> subnodes = NULL;
    b -> next = NULL;
    return b;
}

// djb2 hash function by Dan Berstein
static unsigned int djb2_hash(char* str) {
    unsigned int hash = 5381;
    int c;

    while ( (c = *str++) )
        hash = ((hash << 5) + hash) ^ c;

    return hash;
}

/* Finds the address where the pointer should be (whether or not it is there)
 * So that we can add it if we want
 * returns 0 if it exists, 1 otherwise
 */
static int __get_bucket_addr(hash h, char* key, bucket** ret) {
    unsigned int i = djb2_hash(key) % (h -> size);
    int found = 0;
    bucket* it = &(h -> table)[i];
    bucket* head = it;

    // try to find it in the current bucket
    while(*it && !found) {
        if( strcmp( (*it) -> key, key) == 0 )
            found = 1;
        else
            it = &( (*it) -> next );
    }

    // if we found a bucket, we need to return it
    if(*it)
        *ret = it;
    else
        *ret = head; // otherwise return where it should be (head of the bucket)

    // returning the flag
    return found;
}

static bucket* __get_or_create(hash *h, char* key) {
    bucket new, *ret;
    // if we haven't found it, we will have the head of the bucket where we must add it
    // so we create it, set it to point to the current head (since our new will be the new head)
    // and then we need to set the current head to point to the new head (*ret = new)
    if(! __get_bucket_addr(*h, key, &ret) ) {
        new = new_bucket(key);
        new -> next = *ret;
        ++( (*h) -> n_elements );
        *ret = new;
    }

    // now, either we have the new head all ready with a new bucket
    // or we have the bucket that already existed
    return ret;
}

static int add_subnode(bucket b, int size) {
    if(! b-> subnodes) {
        b -> subnodes = new_hash(size);
        return 1;
    }

    return 0;
}

static void resize_hash(hash *h) {
    int new_size = (*h) -> size * 2;
    hash new = new_hash(new_size);
    new -> n_elements = (*h) -> n_elements;
    for(int i = 0; i < (*h) -> size; i++) {
        bucket b = (*h)->table[i];
        bucket* addr;
        __get_bucket_addr(new, b -> key, &addr);
        *addr = b;
    }

    *h = new;
}

int add_to_hash(hash *h, char* args[], int level) {
    if(level) {
        if((*h) -> n_elements == (*h) -> size)
            resize_hash(h);

        bucket b = *__get_or_create(h, *args);
        int r = add_subnode(b, (*h) -> size);
        add_to_hash(&(b -> subnodes), args + 1, level - 1);
        return r;
    }

    return -1;
}

hash new_hash(int size) {
    hash h = (hash)malloc(sizeof(struct hash));
    h -> size = size;
    h -> n_elements = 0;
    h -> table = (bucket*)calloc(size, sizeof(bucket));
    return h;
}

#ifdef DEBUG
void print_hash(hash h, int level) {
    for(int i = 0; i < h -> size; i++) {
        bucket b = h -> table[i];
        while(b) {
            printf("LEVEL: %d, KEY: %s\n", level, b -> key);
            print_hash(b -> subnodes, level + 1);
            b = b -> next;
        }
    }
}

#endif

#ifdef HASHDEBUG
int main() {
    hash h = new_hash(1);
    char* args[3] = {"Braga", "Braga", "Dume"};
    char* args2[3] = {"Porto", "Porto", "Miragaia"};
    char* args3[3] = {"Braga", "Braga", "Real"};

    add_to_hash(&h, args, 3);
    add_to_hash(&h, args2, 3);
    add_to_hash(&h, args3, 3);
    printf("FINAL SIZE: %d, N_ELEMENTS: %d\n", h -> size, h -> n_elements);
    print_hash(h, 0);
    delete_hash(h);
    return 0;
}
#endif

