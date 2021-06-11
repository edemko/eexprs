#ifndef MAIN_JSON_H
#define MAIN_JSON_H

#include <stdio.h>
#include <string.h>

#include "engine.h"


void fdumpStr(FILE* fp, str text);
void fdumpCStr(FILE* fp, char* s);

void fdumpToken(FILE* fp, const token* tok);
void fdumpLexErr(FILE* fp, const lexError* err);

void fdumpTokenStream(FILE* fp, const char* indent, const tokenStream* root);
void fdumpEexprStream(FILE* fp, const char* indent, const eexprStream* eexprs);
void fdumpLexErrStream(FILE* fp, const char* indent, const lexErrStream* root);

void fdumpLineIndex(FILE* fp, const struct lineIndex* index);


#endif
