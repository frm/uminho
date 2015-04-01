#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mComment.h"

mCommentP initMComment(){
	mCommentP newComment = (mCommentP) malloc(sizeof(mComment));
	newComment-> next = NULL;
    newComment -> cText = NULL;
    newComment -> startLine = -1;
    newComment -> endLine = -1;
    newComment -> language = NULL;
	return newComment;
}

mCommentP newMComment(char* str, int start, int end, char* lang){
	mCommentP newComment = (mCommentP) malloc(sizeof(mComment));
	newComment-> next = NULL;
    newComment -> cText = strdup(str);
    newComment -> startLine = start;
    newComment -> endLine = end;
    newComment -> language = lang;
	return newComment;
}

/*TODO
whatLanguage
*/
