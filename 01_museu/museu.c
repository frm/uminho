#include "museu.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>


PicturePerson* newPicturePerson(PictureGlobal *global){
    PicturePerson *newPicturePerson = (PicturePerson *) malloc(sizeof(PicturePerson));
    newPicturePerson->global = global;
    newPicturePerson->right = NULL;
    newPicturePerson->left = NULL;
    return newPicturePerson;
}

Person* newPerson(char *name, PictureGlobal *picture){
    Person *newPerson = (Person *) malloc(sizeof(Person));
    newPerson->name = strdup(name);
    newPerson->pictures = newPicturePerson(picture);
    newPerson->right = NULL;
    newPerson->left = NULL;

    return newPerson;
}

PicturePerson* addPicToPerson(PicturePerson *start, PictureGlobal *global){
    if(start == NULL)
        return newPicturePerson(global);
    PicturePerson *temp = start;
    if(global->date == NULL){
            while( temp->left != NULL)
                temp = temp->left;
            temp->left = newPicturePerson(global);
            return start;
        }
    while(1){
        if(strcmp( global->date, temp->global->date ) >= 0){
            if(temp->right == NULL){
                temp->right = newPicturePerson(global);
                return start;
            }
            else
                temp = temp->right;
        }
        else{
            if(temp->left == NULL){
                temp->left = newPicturePerson(global);
                return start;
            }
            else
                temp = temp->left;
        }
    }
}

Person* addPerson(char *name, Person *start, PictureGlobal *global){
    if( start == NULL)
        return newPerson(name, global);
    int compare;
    Person *curr = start;
    Person *np;
    while(1){
        compare = strcmp(name, curr->name);
        if(compare == 0){
            curr->pictures = addPicToPerson(curr->pictures, global);
            return start;
        }
        else if(compare > 0){
            if( curr->right == NULL){
                np = newPerson(name, global);
                curr->right = np;
                return start;
            }
            else
                curr = curr->right;
        }
        else{
            if( curr->left == NULL){
                np = newPerson(name, global);
                curr->left = np;
                return start;
            }
            else
                curr = curr->left;
        }
    }
}

PictureGlobal* placePicture(PictureGlobal *pic, PictureGlobal *start){
    if(start == NULL){
        return pic;
    }
    PictureGlobal *temp = start;
    if(pic->date == NULL){
        while( temp->left != NULL)
            temp = temp->left;
        temp->left = pic;
        return start;
    }
    else{
        while(1){
            if(temp->date == NULL || strcmp( pic->date, temp->date ) >= 0){
                if(temp->right == NULL){
                    temp->right = pic;
                    return start;
                }
                else
                    temp = temp->right;
            }
            else{
                if(temp->left == NULL){
                    temp->left = pic;
                    return start;
                }
                else{
                    temp = temp->left;
                }
            }
        }
    }
}

void writePeoplePictures(PicturePerson *p, FILE *f){
    if(p != NULL){
        writePeoplePictures(p->left, f);
        char *date;
        if(p->global->fact == NULL)
            p->global->fact = strdup("Descrição Indisponível");
        if(p->global->date == NULL)
            date = strdup("Data Indisponível");
        else
            date = p->global->date;

        fprintf(f, "<p class=\"fact\">%s</p>\n<h3>%s </h3>\n", p->global->fact, date);
        fprintf(f, "<img src=\"../%s\"/>\n", p->global->path);

        writePeoplePictures(p->right, f);
    }
}

void writePeople(Person *p, FILE *html){
    char *filename;
    struct stat st = {0};
    FILE *newFile;
    if (stat("pessoas/", &st) == -1) {
        mkdir("pessoas/", 0700);
    } 
    if(p != NULL){
        writePeople(p->left, html);
        filename = (char *) calloc(strlen(p->name) + 6, sizeof(char));
        sprintf(filename, "pessoas/%s.html", p->name);
        newFile = fopen(filename, "w+");
        fprintf(newFile, "<!DOCTYPE html>\n<meta charset=\"UTF-8\">\n");
        fprintf(newFile, "<link rel=\"stylesheet\" type=\"text/css\" href=\"../museu.css\" media=\"screen\" />\n");
        fprintf(newFile, "<h1>Fotos de %s</h1>\n", p->name);
        writePeoplePictures(p->pictures, newFile);
        fclose(newFile);

        fprintf(html, "\t\t<li><a href=\"pessoas/%s.html\">%s</a>\n",p->name, p->name);
        writePeople(p->right, html);
    }
}
    
void writePictures(PictureGlobal *p, FILE *html){
    char *date;

    if(p != NULL){
        writePictures(p->left, html);

        if(p->fact == NULL)
            p->fact = strdup("Descrição Indisponível");
        if(p->date == NULL)
            date = strdup("Data Indisponível");
        else
            date = p->date;

        fprintf(html, "<p class=\"fact\">%s</p>\n<h3>%s </h3>\n", p->fact, date);
        fprintf(html, "<img src=\"%s\"/>\n", p->path);
        writePictures(p->right, html);
    }
}
