#include "author_map.h"
#include "heap.h"

#include <strutil.h>
#include <string.h>
#include <avl.h>

#ifdef DEBUG2
static int colisions = 0;
#endif




/* ****** HELPER STRUCTURES FOR HASHMAP ****** */
typedef struct year_info            YearInfo;
typedef struct coauthor             Coauthor;
typedef struct author_info          AuthorInfo;

/* Pair of year and number of publications in given year
 * To be contained inside an author
 */
struct year_info {
    int year;
    int nr_publications;
};

AVL_DEF_HEADER(YearInfo, int)

AVL_DEF(YearInfo, int)

/* Pair of string and int, containing a name of coauthor
 * and number of publications with given author
 */
struct coauthor {
    Author name;
    int nr_publications;
};

AVL_DEF_HEADER(Coauthor, Author)

AVL_DEF(Coauthor, Author)
/* Author Catalog Entry for authors
 * Contains the name of the author,
 * An AVL with info about every author he/she has published with
 * An AVL with info about every year in which he/she has published
 */
struct author_info {
    Author          name;
    CoauthorAVL     coauthors;
    YearInfoAVL     year_info;
};

/* ******** HASHMAP STRUCTURES ******** */

typedef struct bucket_node BucketNode, *Bucket;

struct hash {
    int size;
    Bucket* table;
};

struct bucket_node {
    AuthorInfo          author;
    struct bucket_node* next;
};

/* ***** HEAP FOR TOP AUTHORS IN YEAR ***** */

typedef struct {
    char *str;
    int nr;
} StrIntPair;

HEAP_DEF_HEADER(StrIntPair)
HEAP_DEF(StrIntPair)

static int cmpMinCoAuthorPublPair(StrIntPair fst, StrIntPair snd) {
    int nr_fst, nr_snd;

    nr_fst = fst.nr;
    nr_snd = snd.nr;

    if (nr_fst > nr_snd)
        return -1;
    else if (nr_fst < nr_snd)
        return 1;
    else
        return 0;
}

/* ***************************************** */


/* ******** STATIC FUNCTIONS TO HELP HANDLING YEARINFO AND COAUTHOR TYPES ******** */

/* YearInfo functions */

/* YearInfo is a structure that contains the number of publications in a given
 * It is contained inside an AuthorInfo
 */
static YearInfo yearInfoNew(int year) {
    YearInfo new;
    new.year = year;
    new.nr_publications = 1;
    return new;
}

/* AVL Colision function */
static void colideYearInfo(YearInfo* inTree, YearInfo* outTree) {
    (inTree -> nr_publications) += (outTree -> nr_publications);
}

/* AVL comparison function */
static int compareYearInfo(int* key_search, YearInfo* fst, YearInfo snd) {
    int cmp;
    int key = key_search ? (*key_search) : (fst -> year);

    if ( key > snd.year ) cmp = 1;
    else if ( key < snd.year ) cmp = -1;
    else cmp = 0;

    return cmp;
}

/* Coauthor functions */

/* Coauthor is a structure that contains an author name
 * And the number of publications that the AuthorInfo parent has along with
 */
static Coauthor coauthorNew(Author name) {
    Coauthor new;
    new.name = name;
    new.nr_publications = 1;
    return new;
}

static void deleteCoauthor(Coauthor author) {
    free(author.name);
}

static Coauthor cloneCoauthor(Coauthor original) {
    Coauthor new;
    new.name = str_dup(original.name);
    new.nr_publications = original.nr_publications;
    return new;
}

/* AVL colision function */
static void colideCoauthor(Coauthor* inTree, Coauthor* outTree) {
    (inTree -> nr_publications) += (outTree -> nr_publications);
}

/* AVL Comparison function */
static int compareCoauthor(Author* key_search, Coauthor* fst, Coauthor snd) {
    Author key = key_search ? (*key_search) : (fst -> name);

    return strcmp(key, snd.name);
}

/* Coauthor getters and setters
 * It is safe to return the address without cloning
 * because it is self-contained inside the hash
 */
static Author coAthGetName(Coauthor author) {
    return author.name;
}

static int coAthGetPublications(Coauthor author) {
    return author.nr_publications;
}

static Coauthor coAthSetPublications(Coauthor author, int nr) {
    author.nr_publications = nr;
    return author;
}

static Coauthor coAthSetName(Coauthor author, Author name) {
    author.name = name;
    return author;
}

/* Returning the pointer to a coauthor information node
 * It's safe to return this because it is only accessed by the hash
 */
static int yieldCoauthor(AuthorInfo author, Coauthor* ret) {
    return __avlYield(Coauthor, author.coauthors, ret);
}

/* Author Info functions */

/* AuthorInfo gives us all the informations regarding an author
 * It contains a name and 2 AVLs
 * One of YearInfo and the other one of Coauthors
 */

static AuthorInfo authorInfoCreate(Author name) {
    AuthorInfo new;
    new.name = str_dup(name);
    new.year_info = avlNewComplete(YearInfo, &compareYearInfo, &colideYearInfo, NULL, NULL);
    new.coauthors = avlNewComplete(Coauthor, &compareCoauthor, &colideCoauthor, &deleteCoauthor, &cloneCoauthor);
    return new;
}

/* Self explanatory functions below */
static void authorInfoDelete(AuthorInfo author) {
    free(author.name);
    avlDestroy(YearInfo, author.year_info);
    avlDestroy(Coauthor, author.coauthors);
}

static Author getAuthorInfoName(AuthorInfo author) {
    return author.name;
}

static int has_coauthors(AuthorInfo author) {
    return author.coauthors -> root != NULL;
}

/* Inserts a coauthor to the coauthor avl of the given author */
static int authorInfoAddCoauthor(AuthorInfo author, Author coauthor_name) {
    return avlInsert(Coauthor, author.coauthors, coauthorNew(coauthor_name) ) ;
}
/* Inserts a year info to the year info avl of the given author */
static int authorInfoAddYear(AuthorInfo author, int year) {
    return avlInsert( YearInfo, author.year_info, yearInfoNew(year) );
}

/* Query 3 - returning the number of publications of a given author in a given year */
static int getAuthorPublicationsIn(int year, AuthorInfo author) {
    YearInfo year_info;
    int ret;

    /* Looks for a YearInfo with the given year key
     * avlFind returns and the memory is not allocated,
     * since it will be local to the function, there are no problems regarding encapsulation
     */
    if ( ( ret = avlFind(YearInfo, author.year_info, year, &year_info) ) == 0 )
        ret = year_info.nr_publications;
    else
        ret = 0;

    return ret;
}

/* Returns the number of years in the ret array. fst will store the year represented by ret[0]
 * ret will store the total of publications by year by the coauthor */
static int authorInfoYearVector(AuthorInfo author_info, int *fst, int **ret) {
    int blockSize, test, i, prevYear;
    int *years;
    YearInfo yinfo;

    i = 0;
    blockSize = 100;
    years = (int *)malloc(sizeof(int) * blockSize);

    test = avlYield(YearInfo, author_info.year_info, &yinfo);

    if (test == -1)
        return 0;

    *fst = prevYear = yinfo.year;
    years[0] = yinfo.nr_publications;

    i++;

    while(!test) {
        test = avlYield(YearInfo, author_info.year_info, &yinfo);

        while (prevYear < yinfo.year - 1) {
            if (i == blockSize) {
                blockSize *= 2;
                realloc(years, sizeof(int) * blockSize);
            }
            years[i++] = 0;
            prevYear++;
        }

        if (i == blockSize) {
            blockSize *= 2;
            realloc(years, sizeof(int) * blockSize);
        }

        years[i++] = yinfo.nr_publications;
        prevYear++;
    }

    *ret = years;

    return i;
}

/* Returns 1 if the author published in every year between first and last, otherwise returns 0 */
static int authorInfoPublishedInInterval(AuthorInfo author_info, int first, int last) {
    int i;

    for(i = first; i <= last; i++) {
        if (!avlExists(YearInfo, author_info.year_info, i))
            return 0;
    }

    return 1;
}





/* ********* HASHMAP FUNCTIONS ********* */
/* Hash function created by Dan Bernstein */
static unsigned int djb2( char *str ) {
   unsigned int hash = 5381;
    int c;

        while ( (c = *str++) )
            hash = ((hash << 5) + hash) + c;

        return hash;
}

/* Creating a new node in the hash chain */
static Bucket newBucket(Author name) {
    Bucket new = (Bucket)malloc( sizeof(BucketNode) );
    new -> author = authorInfoCreate(name);
    new -> next = NULL;

    return new;
}

/* Searchs a hash bucket for a given author, returning either the bucket node address (if found) or the head of bucket in **ret
 * The int it returns is either 0 or 1 depending if the author already exists or not
 */
static int HashMapGetAddress(HashMap hash, Author name, Bucket **ret) {
    unsigned int index = djb2(name) % hash->size;
    int found = 0;
    Bucket *it = &hash->table[index];
    Bucket *head = it;


    while ( *it && !found ) {
        if ( strcmp( name, getAuthorInfoName( (*it) -> author ) ) == 0 )
            found = 1;

        else
            it = &((*it) -> next);
    }

    if (! *it)
        *ret = head;
    else
        *ret = it;

    return found;
}

/* Adding an author to the hash
 * Uses GetAddress and if returns 0, it hasn't found the author and needs to add to the head of the bucket
 */
static Bucket *HashMapGetCreate(HashMap hash, Author name) {
    Bucket new;
    Bucket *it;

    if (!HashMapGetAddress(hash, name, &it)){
        new = newBucket(name);
        new -> next = *it;
        *it = new;
    }

    return it;
}

/* Adds an authors coauthor info to the corresponding CoauthorAVL */
static int addCoauthorToAuthor(Bucket* author_node, Author coauthor_name) {
    return authorInfoAddCoauthor( (*author_node) -> author, coauthor_name );
}

/* Adds an authors year info to the corresponding YearInfoAVL */
static int addYearToAuthor(Bucket* author_node, int year) {
    return authorInfoAddYear( (*author_node) -> author, year );
}

/* Aux function that adds an author to an author array (becomes a string matrix) */
static int addToAuthorMatrix(Author* author_matrix, int size, Author new_author) {
    author_matrix[size] = str_dup(new_author);
    return size + 1;
}

/* Clearing the contents of the author matrix and adding a new author */
static int restartAuthorMatrix(Author* author_matrix, int size, Author new_author) {
    int i;

    for (i = 0; i < size; i++)
        free(author_matrix[i]);

    return addToAuthorMatrix(author_matrix, 0, new_author);
}

/* Adds an author to AVL and adds its coauthor info */
int HashMapAddAuthor(HashMap hash, Author author, Author coauthor) {
    return addCoauthorToAuthor( HashMapGetCreate(hash, author), coauthor );
}

/* Gets the address of a given author and adds the yearinfo */
int HashMapAddYear(HashMap hash, Author name, int year) {
    return addYearToAuthor( HashMapGetCreate(hash, name), year );
}

/* Iterating through every hash entry and sum the total of solo authors */
int getSoloAuthors(HashMap hash) {
    int i, sum = 0;
    Bucket it;

    for(i = 0; i < hash->size; i++) {
        it = hash->table[i];

        while (it) {
            if ( !has_coauthors(it -> author) )
                sum++;

            it = it -> next;
        }
    }
    return sum;
}


/* Returning the number of publications of an author in a given year */
int getPublicationsInYear(HashMap hash, int year, Author name) {
    Bucket* it;

    if ( !HashMapGetAddress(hash, name, &it) )
        return -1;

    return getAuthorPublicationsIn(year, (*it) -> author);
}

/* Returns the number of years in the parameter ret used to return the array with publications by year
 * the fstYear parameter is used to return the year represented by the position 0 of the array */
int getAuthorPublByYear(HashMap table, Author author, int *fstYear, int **ret) {
    Bucket *b;

    if (!HashMapGetAddress(table, author, &b))
        return 0;
    else
        return authorInfoYearVector((*b)->author, fstYear, ret);
}

/* Returns an array of strings with the coauthors whom the author most published with */
AuthorArray getCoauthorWithMostPubl(HashMap table, Author author, int* nr_coauthors, int* nr_publications) {
    Bucket* current_author;
    Coauthor coauthor_buffer;
    int avl_empty = 0, max_nr_publications = 0, size = 0, matrix_size = 128;

    Author* author_ary = (Author*)malloc(sizeof(Author) * matrix_size); /* using for realloc cases */

    if( HashMapGetAddress(table, author, &current_author) ) {

        while ( !avl_empty ) {
            avl_empty = yieldCoauthor( (*current_author) -> author, &coauthor_buffer);

            /* Error handling */
            if (avl_empty != -1) {
                /* If we found a new maximum coauthor */
                if ( coAthGetPublications(coauthor_buffer) > max_nr_publications ) {
                    /* Clear the coauthor buffer and add the new coauthor */
                    size = restartAuthorMatrix(author_ary, size, coAthGetName(coauthor_buffer)); /* get the new size */
                    max_nr_publications = coAthGetPublications(coauthor_buffer); /* update comparison variable */
                }
                /* If we found someone that has the same number of publications as the current max */
                else if ( coAthGetPublications(coauthor_buffer) == max_nr_publications ) {
                    size = addToAuthorMatrix(author_ary, size, coAthGetName(coauthor_buffer));
                    /* If by adding we reach the buffer limit */
                    if (size == matrix_size) {
                        author_ary = realloc(author_ary, sizeof(Author*) * matrix_size * 2);
                        matrix_size *= 2;
                    }
                }
            }
        }
    }

    *nr_coauthors = size; /* Returning the size of the buffer */
    *nr_publications = max_nr_publications; /* Returning the nr of publications of max author */
    return author_ary;
}

/* Returns the number of authors in the parameter ret used to return the array with the n most published
 * authors in a given year*/
int getTopAuthorsInYear(HashMap hash, int year, int n, Author **ret) {
    StrIntPairHeap heap;
    StrIntPair author_publ, worst_author_publ;
    Bucket it;
    int i, count;
    char **list;

    count = 0;
    i = 0;
    heap = heapNew(StrIntPair, n, &cmpMinCoAuthorPublPair);
    list = (char **)malloc(sizeof(char *) * n);

    while (count < n && i < hash->size) {
        it = hash->table[i];

        while (it) {

            author_publ.nr = getAuthorPublicationsIn(year, it->author);
            author_publ.str = getAuthorInfoName(it->author);

            if (count < n){
                if (author_publ.nr > 0){
                    heapInsert(StrIntPair, heap, author_publ);
                    count++;
                }
            }
            else{
                heapTop(StrIntPair, heap, &worst_author_publ);

                if (author_publ.nr > worst_author_publ.nr) {
                    heapGet(StrIntPair, heap, &worst_author_publ);
                    heapInsert(StrIntPair, heap, author_publ);
                }
            }
            it = it->next;
        }

        worst_author_publ.nr = -1;

        i++;
    }

    for(; i < hash->size; i++) {
        it = hash->table[i];

        while (it) {
            author_publ.nr = getAuthorPublicationsIn(year, it->author);
            author_publ.str = getAuthorInfoName(it->author);

            if (worst_author_publ.nr == -1)
                heapTop(StrIntPair, heap, &worst_author_publ);

            if (author_publ.nr > worst_author_publ.nr) {
                heapGet(StrIntPair, heap, &worst_author_publ);
                heapInsert(StrIntPair, heap, author_publ);
                worst_author_publ.nr = -1;
            }

            it = it->next;
        }
    }

    for (i = count - 1; i >= 0; i--) {
        heapGet(StrIntPair, heap, &author_publ);
        list[i] = (char *)malloc(sizeof(char) * strlen(author_publ.str) + 1);
        strcpy(list[i], author_publ.str);
    }

    heapDestroy(StrIntPair, heap);

    *ret = list;

    return count;
}

/* Returns the number of authors in the parameter ret used to return the array with the authors who
 * published every year in the interval [fst, snd] */
int getAuthorsByInterval(HashMap hash, int fst, int snd, Author **ret) {
    int i, count, block;
    Bucket it;
    Author *list;

    count = 0;
    block = 2000;
    list = (Author *)malloc(sizeof(Author) * block);

    for(i = 0; i < hash->size; i++) {
        it = hash->table[i];

        while (it) {
            if ( authorInfoPublishedInInterval(it->author, fst, snd) ){
                if (count == block) {
                    block *= 2;
                    list = (Author *)realloc(list, sizeof(Author) * block);
                }
                list[count] = str_dup( getAuthorInfoName(it->author) );
                count++;
            }
            it = it -> next;
        }
    }

    *ret = list;

    return count;
}

/* Deletes a chain of colision in the Hash */
static void deleteBucket(Bucket b) {
    Bucket del, it = b;
    while (it) {
        del = it;
        it = it -> next;
        authorInfoDelete(del -> author);
        free(del);
    }
}

void deleteHashMap(HashMap hash) {
    int i;

    for(i = 0; i < hash->size; i++)
        deleteBucket( hash->table[i] );

    free(hash->table);
    free(hash);
}

HashMap newHashMap(int size) {
    HashMap hash;

    hash = (HashMap)malloc(sizeof(struct hash));

    hash->size = size;
    hash->table = (Bucket*)calloc( size, sizeof(Bucket) );

    return hash;
}
