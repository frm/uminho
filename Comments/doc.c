#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "doc.h"


void writeCommentsByTag(FILE *ff, Tag *tt){
    if(tt != NULL){
        writeCommentsByTag(ff, tt->left);
        fprintf(ff, "AUTOR: %s\n", tt->name);
        AuxDocComment *comment = tt->comments;
        while(comment != NULL){
            fprintf(ff, "\t\tCOMMENT: %s", comment->doc->comment);
        }
        writeCommentsByTag(ff, tt->right);
    }
}


char *getTag(char *comment, char *tagName){
    comment = comment+3;
    int i = 0;
    int foundTag = 0;
    char *result;

    if(comment[i] == '@'){
        for(i = 1; i<strlen(tagName) && comment[i] == tagName[i]; i++);
        comment = comment+i+1;
        if( i == strlen(tagName)) foundTag = 1;
    }

    for( i = 0; comment[i] != '\0' && comment[i] != '\n'; i++);

    if(foundTag && i > 0){
        if( comment[i] == '\n')
            comment[i] = '\0';

        result = strdup(comment);

        comment[i] = '\n';

        return result;
    }

    if( comment[i] == '\0')
        return NULL;

    return getTag(comment+i+1, tagName);
}



Tag *newTag(char *name){
    Tag *newA = (Tag *) malloc(sizeof(Tag));
    newA->name = name;
    newA->comments = NULL;
    newA->left = NULL;
    newA->right = NULL;
    return newA;
}

DocComment* newDocComment(char *comment){
    DocComment *newDoc = (DocComment *) malloc(sizeof(DocComment));
    newDoc->comment = comment;
    newDoc->next = NULL;
    return newDoc;
}

DocComment* addDocComment(DocComment *start, DocComment *comment){
    if(start == NULL)
        return comment;
    DocComment *temp = start;
    while(temp->next != NULL){
        temp = temp->next;
    }
    temp->next = comment;
    return start;
}

AuxDocComment * newAuxDocComment(DocComment *doc){
    AuxDocComment *newAux = (AuxDocComment *) malloc( sizeof(AuxDocComment ) );
    newAux->doc = doc;
    newAux->next = NULL;
    return newAux;
}

AuxDocComment * addAuxDocComment(AuxDocComment *start, AuxDocComment *aux){
    if(start == NULL)
        return aux;
    AuxDocComment *temp = start;
    while(temp->next != NULL)
        temp = temp->next;
    temp->next = aux;
    return start;
}

Tag *addTag( char *name, Tag *start, DocComment *comment){
    if(start == NULL)
        return newTag(name);
    Tag *curr = start;
    Tag *newA;
    int compare;
    
    while(1){
        compare = strcmp(name, curr->name);
        if(compare == 0){
            curr->comments = addAuxDocComment(curr->comments, newAuxDocComment(comment));
            return start;
        }
        else if(compare > 0){
            if( curr->right == NULL){
                newA = newTag(name);
                newA->comments = addAuxDocComment(newA->comments, newAuxDocComment(comment) );
                curr->right = newA;
                return start;
            }
            else
                curr = curr->right;
        }
        else{
            if( curr->left == NULL){
                newA = newTag(name);
                newA->comments = addAuxDocComment(newA->comments, newAuxDocComment(comment));
                curr->left = newA;
                return start;
            }
            else
                curr = curr->left;
        }
    }
}

