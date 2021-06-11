#ifndef PARSER_UTIL_H
#define PARSER_UTIL_H

#include "engine.h"
#include "lexer/util.h"
#include "types.h"


//////////////////////////////////// Parser State ////////////////////////////////////

struct eexprStream {
  size_t cap;
  size_t len;
  eexpr buf[];
};

void eexprStream_del(eexprStream* strm);


//////////////////////////////////// Functions ////////////////////////////////////

token* parser_peek(parser* st);
// assumes that someone else has taken ownership of the token's data
void parser_pop(parser* st);

void parser_addEexpr(parser* st, const eexpr* expr);

static inline void parser_addErr(parser* st, const lexError* err) { lexer_addErr(st, err); }
static inline void parser_fatalErr(parser* st, const lexError* err) { lexer_fatalErr(st, err); }


#endif
