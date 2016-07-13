#ifndef _MUSEU_H
#define _MUSEU_H

#include <stdio.h>


typedef struct picGlobal{
    char *path;
    char *date;
    char *fact;
    struct picGlobal *left;
    struct picGlobal *right;
}PictureGlobal;

typedef struct picPerson{
    PictureGlobal *global;
    struct picPerson *left;
    struct picPerson *right;
}PicturePerson;

typedef struct per{
    char *name;
    PicturePerson *pictures;
    struct per *left;
    struct per *right;
}Person;

Person* addPerson(char *name, Person *start, PictureGlobal *global);

PictureGlobal* placePicture(PictureGlobal *pic, PictureGlobal *start);

void writePictures(PictureGlobal *p, FILE *html);

void writePeople(Person *p, FILE *html);

#endif