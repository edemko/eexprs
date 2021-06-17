#ifndef INTERNAL_ENGINE_H
#define INTERNAL_ENGINE_H

#include "types.h"
#include "parameters.h"

#define TYPE eexpr_token
#include "dllist.h"

#define TYPE eexpr_error
#include "dllist.h"


//////////////////////////////////// Lexer State ////////////////////////////////////

typedef struct openWrap {
  eexpr_wrapType type;
  eexpr_loc loc;
} openWrap;

#define TYPE openWrap
#include "dynarr.h"

typedef struct engine {
  str rest; // borrowed pointer to input
  struct eexpr_locPoint loc; // use zero-indexed line/col and only translate to 1-indexd for human consumption
  dllist_eexpr_token tokStream; //owned
  dynarr_eexpr_p eexprStream; //owned
  dllist_eexpr_error errStream; // owned
  eexpr_error fatal; // use EEXPRERR_NOERROR for no error
  newlineType discoveredNewline; // NEWLINE_NONE if not set
  struct lexer_indent {
    bool knownMixed;
    char32_t chr; // UCHAR_NULL when indentation is not yet established
    eexpr_loc established;
  } indent;
  dynarr_openWrap wrapStack;
} engine;

//////////////////////////////////// General Functions ////////////////////////////////////

// Initialize from a sized string.
engine engine_newFromStrn(size_t n, uint8_t* input);


// free all internal data structures of the passed engine
void engine_deinit(engine* st);

void engine_rawLex(engine* st);
void engine_cookLex(engine* st);
void engine_parse(engine* st);


//////////////////////////////////// Lexer/Postlexer Helper Functions ////////////////////////////////////

void lexer_advance(engine* st, size_t bytes, size_t cols);

void lexer_incLine(engine* st, size_t bytes);

// `lexer_addTok` and `lexer_insertBefore` ensure that added tokens are non-transparent
void lexer_addTok(engine* st, const eexpr_token* t);
void lexer_insertBefore(engine* st, const eexpr_token* t, dllistNode_eexpr_token* point);

// remove the last token (useful for re-using standard `take*` procedures as part of others)
// ensures the memory used by that token is also deallocated
void lexer_delTok(engine* st);


//////////////////////////////////// Parser Helper Functions ////////////////////////////////////


// both `parser_peek` and `parser_pop` destroy transparent tokens from the start of the token stream, along with their token data.

// returns a borrowed pointer to the first non-transparent token
eexpr_token* parser_peek(engine* st);

// Removes the first non-transparent token from the stream.
// It does not free any token data, so you must assume ownership of the popped token's data before popping.
// For the foreseeable future, this should be easy, since the `malloc`d data of a token is needed to populate the data of an eexpr.
void parser_pop(engine* st);


#endif
