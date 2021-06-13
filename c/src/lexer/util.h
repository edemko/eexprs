#ifndef LEXER_UTIL_H
#define LEXER_UTIL_H

#include "engine.h"
#include "parameters.h"
#include "types.h"


void lexer_advance(parser* st, size_t bytes, size_t cols);

void lexer_incLine(parser* st, size_t bytes);

// `lexer_addTok` and `lexer_insertBefore` ensure that added tokens are non-transparent
void lexer_addTok(parser* st, const token* t);
void lexer_insertBefore(parser* st, const token* t, dllistNode(token)* point);

// remove the last token (useful for re-using standard `take*` procedures as part of others)
// ensures the memory used by that token is also deallocated
void lexer_delTok(parser* st);


#endif
