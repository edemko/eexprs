#ifndef PARSER_UTIL_H
#define PARSER_UTIL_H

#include "engine.h"
#include "lexer/util.h"
#include "types.h"


//////////////////////////////////// Functions ////////////////////////////////////

// both parser_peek and parser_pop destroy transparent tokens from the start of the token stream, along with their token data.

// returns a borrowed pointer to the first non-transparent token
token* parser_peek(parser* st);
// Removes the first non-transparent token from the stream.
// It does not free any token data, so you must assume ownership of the popped token's data before popping.
void parser_pop(parser* st);


#endif
