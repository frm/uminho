%{
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include "linkedList.h"

SongP p;
char *title, *author, *lyrics, *music, *singer, *lyricsText;


void processSong(){
    if(yytext[yyleng-1] == '-')
        yytext[yyleng-4] = '\0';

    lyricsText = strdup(yytext + 2);

    SongP new = newSong(title,author,lyrics,music,singer,lyricsText);

    new -> next = p;
    p = new;

}    

%}
%%

(?i:title):" ".*        {title = strdup(yytext + 7);}
(?i:author):" ".*       {author = strdup(yytext + 8);}
(?i:lyrics):" ".*       {lyrics =strdup(yytext + 8);}
(?i:music):" ".*        {music = strdup(yytext + 7);}
(?i:singer):" ".*       {singer = strdup(yytext + 8);}

\n\n[^#]+(##-|[^#])     { processSong(); }

.|\n                    ;

%%

int main(int argc, char** argv) {
    if (argc == 2)
        yyin = fopen(argv[1], "r");

    p = initSong();

    yylex();

    struct stat t;
    if(stat("cancoes", &t) == -1)
        mkdir("cancoes", 0700);

    while(p->next != NULL){
        /*creates document name, taking the song name into account*/
        char buffer[100];
        snprintf(buffer, 100, "./cancoes/%s.tex", p->title);
        FILE *fp = fopen(buffer,"w+");

        fprintf(fp, "\\documentclass[12pt]{article}\n\\usepackage[utf8]{inputenc}\n\\date{}\n");
        
        /*checks what type of author should be written in the file*/
        if(p->author != NULL)
            fprintf(fp, "\\author{%s}\n",p->author);
        else if(p->lyrics != NULL && p->music != NULL)
            fprintf(fp, "\\author{%s ; %s}\n",p->lyrics,p->music);

        /*title and lyrics text. I use verbatim so I don't have to worry with \n and whitespaces*/
        fprintf(fp, "\\title{%s}\n\\begin{document}\n\\maketitle\n\\section*{Letra}\n\\begin{verbatim}\n%s\n\\end{verbatim}\n",p->title, p->lyricsText);

        /*writes singer to file, centered to the right*/
        if(p->singer != NULL)
            fprintf(fp, "\\begin{flushright}\n%s\n\\end{flushright}\n",p->singer);

        fprintf(fp,"\\end{document}");
        fclose(fp);

        p = p->next;
    }

    return 0;
}