branco [ \t]*

%s fotos

%{
    #include "museu.h"
    #include "util.h"

    FILE *html;
    PictureGlobal *pictures = NULL;
    PictureGlobal *currPicture = NULL;
    Person *people = NULL;


    void addPath(char *p){
        PictureGlobal *newPic = (PictureGlobal *) malloc( sizeof(PictureGlobal));
        newPic->path = strdup(p);
        newPic->fact = NULL;
        newPic->date = NULL;
        newPic->left = NULL;
        newPic->right = NULL;
        currPicture = newPic;
    }

    void addDate(char *d){
        if(currPicture != NULL){
            currPicture->date = trim_str( strdup(d) );
        }
    }

    void addFact(char *f){
        if(currPicture != NULL)
            currPicture->fact = strdup(f);
    }

    void addPeopleLine(char *line){
        char *newStr = strtok( strcut(line, '>', '<'), ";");
        while( newStr != NULL){
        people = addPerson(  str_repl( trim_str(newStr), "\n\t.", ' ' ), people, currPicture);
        newStr = strtok(NULL, ";");
        }
    }

    void insertCurrPicture(){
        pictures = placePicture(currPicture, pictures);
        currPicture = NULL;
    }

%}

%%


\<{branco}fotos{branco}\>                                               BEGIN fotos;

<fotos>\<{branco}\/fotos{branco}\>                                      BEGIN 0;

<fotos>\<{branco}quem{branco}\>[^<]+\<{branco}\/quem{branco}\>          addPeopleLine(yytext);

<fotos>\<{branco}foto[ \t][^>]*ficheiro=\"[^"]+\"                       addPath( getAttributeValue(yytext) );

<fotos>\<quando[ \t][^>]*data=\"[^"]+\"                                 addDate( trim_str(getAttributeValue(yytext)) );

<fotos>\<{branco}facto{branco}\>[^<]+\<{branco}\/facto{branco}\>        addFact( strcut(yytext, '>', '<'));

<fotos>\<{branco}\/foto{branco}\>                                       if(currPicture != NULL) insertCurrPicture();

.|\n                                                               ;


%%

int main(int argc, char** argv){
    if (argc == 2)
        yyin = fopen(argv[1], "r");

    html = fopen("album.html", "w+");

    if (html == NULL)
        exit(1);

    yylex();
    fprintf(html, "<!DOCTYPE html>\n<meta charset=\"UTF-8\">\n");
    fprintf(html, "<link rel=\"stylesheet\" type=\"text/css\" href=\"museu.css\" media=\"screen\" />\n");
    fprintf(html, "<h1> Pessoas Neste Álbum </h1>\n");

    fprintf(html, "<div id=\"people-list\">\n");
    fprintf(html, "\t<ul>\n"); 
    writePeople(people, html);
    fprintf(html, "\t</ul>\n");
    fprintf(html, "</div>\n");

    fprintf(html, "<h1> Fotos </h1>\n");
    
    writePictures(pictures, html);



    fclose(html);
    return 0;
}  
