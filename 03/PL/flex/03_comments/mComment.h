#ifndef _MCOMMENT_H
#define _MCOMMENT_H
typedef struct mComment{
	int startLine;
	int endLine;
	char* language;
	char *cText;
	struct mComment *next;
} *mCommentP, mComment;

mCommentP initMComment();

mCommentP newMComment(char* str, int start, int end, char* lang);

#endif
