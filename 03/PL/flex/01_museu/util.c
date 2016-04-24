#include "util.h"
#include <stdlib.h>
#include <string.h>

char * getAttributeValue(char *str){
    int i;
    int length = strlen(str);
    str[length-1] = '\0';

    for( i = length-2; i >= 0; i--){
        if(str[i]=='\"')
            return &str[i+1];
    }
    return 0;
}

char* strcut(char* str, char fst, char snd) {
    if(! str)
        return NULL;

    int start = -1, end = 0, i = 0;
    int size = strlen(str);

    for(i = 0; i < size && str[i] != fst; i++);

    start = i + 1;

    for(++i; i < size && str[i] != snd; i++);

    end = i;

    if(start >= size || end >= size)
        return NULL;

    int mv = end - start + 1;
    char* s = (char*)malloc( sizeof(char) * mv );
    memmove(s, str + start, mv);
    s[mv - 1] = '\0';

    return s;
}


char * trim_str(char *s){
    while( *s == ' ' ) s++;
    int i = strlen(s) - 1 ;
    while( s[i] == ' ' ) i--;
    s[i+1] = '\0';
    return s;
}

char * str_repl(char *str, char *chr, char repl){
     int i=0, j = 0;
     char c;
     while(str[i]!='\0')
     {  
        j = 0;
        while( (c = chr[j]) != '\0'){
           if(str[i]==c)
           {
               str[i]=repl;
           }  
           j++; 
        }
        i++;
     }
     return str;
}
