#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "doc.h"





void writeAuthors(Tag *author, FILE *index){
    char *filename;
    struct stat st = {0};
    FILE *newFile;
    if (stat("autores/", &st) == -1) {
        mkdir("autores/", 0700);
    } 
    if(p != NULL){
        writeAuthors(author->left, index);
        filename = (char *) calloc(strlen(author->name) + 17, sizeof(char));
        sprintf(filename, "autores/aut_%s.html", author->name);
        newFile = fopen(filename, "w+");
        fprintf(newFile, "<!DOCTYPE html>\n<meta charset=\"UTF-8\">\n");
        fprintf(newFile, "<link rel=\"stylesheet\" type=\"text/css\" href=\"../comment.css\" media=\"screen\" />\n");
        fprintf(newFile, "<h1>Comentários feito por %s</h1>\n", author->name);
        writeAuxComments()
        writePeoplePictures(p->pictures, newFile);
        fclose(newFile);
        free(filename);

        fprintf(html, "\t\t<li><a href=\"pessoas/%s.html\">%s</a>\n",p->name, p->name);
        writePeople(authors->right, html);
    }
}

void writeVersionsToFile(Tag *versions){

}

char *getTag(char *comment, char *tagName){
    comment = strstr(comment, tagName);
    char *result;

    if(comment == NULL)
        return NULL;

    comment += strlen(tagName) + 1;

    int i;

    for( i = 0; comment[i] != '\0' && comment[i] != '\n'; i++);

    if( comment[i] == '\n'){
        comment[i] = '\0';
        result = strdup(comment);
        comment[i] = '\n';
    }
    else
        result = strdup(comment);

    return result;




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
    if(start == NULL){
        AuxDocComment *newAuxDoc = newAuxDocComment(comment);
        Tag *result =  newTag(name);
        result->comments = newAuxDoc;
        printf("\nAIAAIAIAIAIAA\n");
        return result;
    }
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

