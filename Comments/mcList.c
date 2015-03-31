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

typedef struct mCommentList{
	struct mComment *cursor;
} *mCommentListP, mCommentList;

mCommentListP init(){
	mCommentListP res = (mCommentListP) malloc(sizeof(mCommentList));
	res->cursor = NULL;
	return res;
}

mCommentP newMComment(){
	mCommentP newComment = (mCommentP) malloc(sizeof(mComment));
	newComment->next = NULL;
	return newComment;
}

void addMComment(mCommentListP mcl, mCommentP mc){
	mc->next = mcl->cursor;
	mcl->cursor = mc;
}

/*TODO
whatLanguage
*/
