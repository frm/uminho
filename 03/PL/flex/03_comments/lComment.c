#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lComment.h"

lCommentP initLComment(){
	lCommentP newComment = (lCommentP) malloc(sizeof(lComment));
	newComment-> next = NULL;
    newComment -> cText = NULL;
    newComment -> line = -1;
	return newComment;
}

lCommentP newLComment(char* str, int l){
	lCommentP newComment = (lCommentP) malloc(sizeof(lComment));
	newComment-> next = NULL;
    newComment -> cText = strdup(str);
    newComment -> line = l;
	return newComment;
}
