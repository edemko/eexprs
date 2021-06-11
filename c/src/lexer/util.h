#ifndef LEXER_UTIL_H
#define LEXER_UTIL_H

#include "engine.h"
#include "parameters.h"
#include "types.h"


//////////////////////////////////// Lexer State ////////////////////////////////////

struct tokenStream {
  token here; // non-null
  tokenStream* next; // owned, null for end of stream
  tokenStream* prev; // aliased, null for start of stream
};
struct lexErrStream {
  lexError here; // non-null
  lexErrStream* next; // owned, null for end of stream
  lexErrStream* prev; // aliased, null for start of stream
};

void tokenStream_del(tokenStream* strm);

void lexErrStream_del(lexErrStream* strm);


//////////////////////////////////// Functions ////////////////////////////////////

void lexer_advance(parser* st, size_t bytes, size_t cols);

void lexer_incLine(parser* st, size_t bytes);


void lexer_addTok(parser* st, const token* t);

void lexer_insertBefore(parser* st, const token* t, tokenStream* point);

// remove the last token (useful for re-using standard `take*` procedures as part of others)
void lexer_delTok(parser* st);


void lexer_addErr(parser* st, const lexError* err);

void lexer_fatalErr(parser* st, const lexError* err);

// move an error into the warnings list
void lexer_errToWarn(parser* st, lexErrStream* err);


#endif
