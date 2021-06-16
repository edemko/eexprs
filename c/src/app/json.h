#ifndef APP_JSON_H
#define APP_JSON_H

#include <stdio.h>
#include <string.h>

#include "eexpr.h"
#include "shim/strstuff.h"


void fdumpStr(FILE* fp, str text);
void fdumpCStr(FILE* fp, char* s);

void fdumpToken(FILE* fp, const eexpr_token* tok);
void fdumpError(FILE* fp, const eexpr_error* err);

void fdumpTokenArray(FILE* fp, const char* indent, size_t n, eexpr_token** arr);
void fdumpEexprArray(FILE* fp, int indent, size_t n, eexpr** xs);
void fdumpErrorArray(FILE* fp, const char* indent, size_t n, eexpr_error* arr);


#endif
