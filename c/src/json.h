#ifndef JSON_H
#define JSON_H

#include <stdio.h>

#include "lexer.h"


void fdumpTokens(FILE* fp, tokenStream* strm);
void fdumpLexerrs(FILE* fp, lexErrStream* strm);

#endif
