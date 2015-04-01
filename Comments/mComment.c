#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mComment.h"

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
