#ifndef _MUSEU_H
#define _MUSEU_H

typedef struct doc{
    char *comment;
    char *firstLine;
    struct doc *next;
}DocComment;

typedef struct auxDoc{
    DocComment *doc;
    struct auxDoc *next;
}AuxDocComment;

typedef struct tag{
    char *name;
    AuxDocComment *comments;
    struct tag *left;
    struct tag *right;
}Tag;

void writeCommentsByTag(FILE *ff, Tag *tt);

char *getTag(char *comment, char *tagName);

Tag *addTag( char *name, Tag *start, DocComment *comment);

DocComment* newDocComment(char *comment);

DocComment* addDocComment(DocComment *start, DocComment *comment);

void writeAuthors(Tag *author, FILE *index);

void writeVersions(Tag *version, FILE *index);

void writeDocComments(DocComment *doc);

#endif
