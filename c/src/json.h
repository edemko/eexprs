#ifndef JSON_H
#define JSON_H

#include <stdio.h>

#include "lexer.h"


void fdumpTokens(FILE* fp, tokenStream* strm);
void fdumpLexErrs(FILE* fp, lexErrStream* strm);

#endif
