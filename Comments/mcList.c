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

mCommentP newMComment(){
	mCommentP newComment = (mCommentP) malloc(sizeof(mComment));
	newComment->next = NULL;
	return newComment;
}

void addMComment(mCommentP base, mCommentP mc){
	mc->next = base;
	base = mc;
}

/*TODO
whatLanguage
*/
