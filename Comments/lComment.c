#include <stdio.h>
#include <stdlib.h>
#include <string.h>


typedef struct lComment{
	int line;
	char *cText;
	struct lComment *next;
} *lCommentP, lComment;

lCommentP newLComment(char* str, int l){
	lCommentP newComment = (lCommentP) malloc(sizeof(lComment));
	newComment-> next = NULL;
    newComment -> cText = strdup(str);
    newComment -> line = l;
	return newComment;
}