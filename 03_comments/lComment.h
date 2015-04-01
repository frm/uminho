#ifndef _LCOMMENT_H
#define _LCOMMENT_H
typedef struct lComment{
	int line;
	char *cText;
	struct lComment *next;
} *lCommentP, lComment;

lCommentP initLComment();

lCommentP newLComment();

#endif
