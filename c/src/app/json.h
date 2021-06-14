#ifndef APP_JSON_H
#define APP_JSON_H

#include <stdio.h>
#include <string.h>

#include "engine.h"


void fdumpStr(FILE* fp, str text);
void fdumpCStr(FILE* fp, char* s);

void fdumpToken(FILE* fp, const token* tok);
void fdumpError(FILE* fp, const eexpr_error* err);

void fdumpTokenArray(FILE* fp, const char* indent, size_t n, token** arr);
void fdumpEexprArray(FILE* fp, int indent, const dynarr_eexpr_p* eexprs);
void fdumpErrorArray(FILE* fp, const char* indent, size_t n, eexpr_error* arr);

void fdumpLineIndex(FILE* fp, size_t len, size_t* offsets);


#endif
