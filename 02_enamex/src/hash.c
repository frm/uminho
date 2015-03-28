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

static int empty_bucket(bucket b) {
    return b -> subnodes -> n_elements == 0;
}

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
    char* lower_key = str_to_lower(key);
    printf("\n\n### KEY: %s\nLOWER: %s\n", key, lower_key);
    unsigned int i = djb2_hash(lower_key) % (h -> size);
    free(lower_key);
    int found = 0;
    bucket* it = &(h -> table)[i];
    bucket* head = it;

    // try to find it in the current bucket
    while(*it && !found) {
        if( strcasecmp( (*it) -> key, key) == 0 )
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

static char* __bucket_paragraph(bucket b) {
    // 38: <p></p> + \0
    int size = strlen(b -> key) + 36;
    char* contents = (char*)calloc(size, sizeof(char));
    sprintf(contents, "<div class=\"large-12\"><p>%s</p></div>", b -> key);
    return contents;
}

static char* __subhash_to_html(hash h, int level) {
    if(!h)
        return NULL;

    int header = level < 6 ? level : 6;
    char* contents = (char*)calloc(1, sizeof(char));
    for(int i = 0; i < h -> size; i++) {
        bucket b = h -> table[i];

        if(!b)
            continue;

        char* new_contents;

        if( empty_bucket(b) ) {
            new_contents = __bucket_paragraph(b);
            int size = strlen(contents) + strlen(new_contents) + 1;
            contents = (char*)realloc(contents, size);
            sprintf(contents, "%s%s", contents, new_contents);
            continue;
        }

        else new_contents = __subhash_to_html(b -> subnodes, level + 2);

        if(new_contents) {
            // 40: <div class="large-12"><h?></h?></div> + \0
            int size = strlen(contents) + strlen(new_contents) + strlen(b -> key) + 48;
            contents = (char*)realloc(contents, size);
            sprintf(contents, "%s<div class=\"large-12 accordion\"><h%d>%s</h%d>%s</div>", contents, header, b -> key, header, new_contents);
        }
    }
    return contents;
}

static char* __hash_to_html(bucket b) {
    if(!b)
        return NULL;
    // 33: <div class="large-12"><h1></h1> + \0
    /*char* contents = (char*)malloc(sizeof(char) * (strlen(b -> key) + 33) );
    sprintf(contents, "<div class=\"large-12\"><h1>%s</h1>", b -> key);*/
    char* contents = (char*)calloc(1, sizeof(char));

    char* new_contents = __subhash_to_html(b -> subnodes, 3);
    if(new_contents) {
        // 37: <div class="large-12"></div> + \0
        int size = strlen(contents) + strlen(new_contents) + 31;
        char* str = (char*)malloc(sizeof(char) * size);
        sprintf(str, "%s<div class=\"large-12\">%s</div>", contents, new_contents);

        return str;
    }

    return NULL;
}

char* hash_to_html(hash h) {
    if(!h)
        return NULL;

    char* contents = (char*)calloc(1, sizeof(char));
    for(int i = 0; i < h -> size; i++) {
        bucket b = h -> table[i];
        char* new_contents = __hash_to_html(b);

        // 40: <div class="large-12"><h1></h1></div> + \0
        if(new_contents) {
            int size = strlen(contents) + strlen(new_contents) + strlen(b -> key) + 48;
            contents = (char*)realloc(contents, size);
            sprintf(contents, "%s<div class=\"large-12 accordion\"><h1>%s</h1>%s</div>", contents, b -> key, new_contents);
        }

   }
    // 24 : <div class="row"></div> + \0
/*    int final_size = strlen(contents) + 24;
    contents = (char*)realloc(contents, final_size);
    sprintf(contents, "<div class=\"row\">%s</div>", contents);*/
    return contents;
}


static void __fprint_hash(FILE* f, hash h, int level) {
     for(int i = 0; i < h -> size; i++) {
        bucket b = h -> table[i];
        while(b) {
            fprintf(f, "LEVEL: %d, KEY: %s\n", level, b -> key);
            __fprint_hash(f, b -> subnodes, level + 1);
            b = b -> next;
        }
    }

}
void fprint_hash(FILE* f, hash h) {
    __fprint_hash(f, h, 0);
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
