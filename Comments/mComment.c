#include <stdio.h>
#include <stdlib.h>
#include <string.h>


typedef struct mComment{
	int startLine;
	int endLine;
	int language;
	char *cText;
	struct mComment *next;
} *mCommentP, mComment;

mCommentP newMComment(char* str, int start, int end){
	mCommentP newComment = (mCommentP) malloc(sizeof(mComment));
	newComment-> next = NULL;
    newComment -> cText = strdup(str);
    newComment -> startLine = start;
    newComment -> endLine = end;
	return newComment;
}

/*TODO
whatLanguage
*/
