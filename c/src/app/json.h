#ifndef APP_JSON_H
#define APP_JSON_H

#include <stdio.h>
#include <string.h>

#include "engine.h"


void fdumpStr(FILE* fp, str text);
void fdumpCStr(FILE* fp, char* s);

void fdumpToken(FILE* fp, const token* tok);
void fdumpError(FILE* fp, const eexprError* err);

void fdumpTokenStream(FILE* fp, const char* indent, const dllistNode(token)* root);
void fdumpEexprStream(FILE* fp, const char* indent, const dynarr(eexprPtr)* eexprs);
void fdumpErrorStream(FILE* fp, const char* indent, const dllistNode(eexprError)* root);

void fdumpLineIndex(FILE* fp, const struct lineIndex* index);


#endif
