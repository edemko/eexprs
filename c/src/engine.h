#ifndef ENGINE_H
#define ENGINE_H

#include "types.h"
#include "parameters.h"


//////////////////////////////////// Lexer State ////////////////////////////////////

typedef struct tokenStream tokenStream;
typedef struct lexErrStream lexErrStream;
typedef struct eexprStream eexprStream;

typedef struct parser {
  str rest; // alias into .allInput
  filelocPoint loc; // use zero-indexed line/col and only translate to 1-indexd for human consumption
  tokenStream* tokStream; // owned, null for empty stream
  tokenStream* tokStream_end; // aliased by `.tokStream`, null for empty stream
  eexprStream* eexprStream; // owned, null for empty stream
  eexprStream* eexprStream_end; // aliased by `.eexprStream`, null for empty stream
  lexErrStream* warnStream; // owned, null for empty stream
  lexErrStream* warnStream_end; // aliased by `.errStream`, null for empty stream
  lexErrStream* errStream; // owned, null for empty stream
  lexErrStream* errStream_end; // aliased by `.errStream`, null for empty stream
  lexError* fatal; // owned, null for no fatal error
  newlineType discoveredNewline; // NEWLINE_NONE if not set
  struct lexer_indent {
    bool knownMixed;
    uchar chr;
    fileloc established;
  } indent;
  str allInput; // owned
  struct lineIndex {
    size_t cap;
    size_t len;
    size_t* offsets;
  } lineIndex;
} parser;
typedef parser lexer;

//////////////////////////////////// Functions ////////////////////////////////////

/*
Initialize a lexer state from a file.
On error, returned `lexer.rest.bytes` is `NULL`.
Ownership of `filename` is borrowed.
*/
parser parser_newFromFile(const char* filename);

// free all internal data structures of the passed parser
void parser_del(parser* st);

void lexer_raw(parser* st);
void lexer_cook(parser* st);
void parser_parse(parser* st);


// static inline void parser_addTok(parser* st, const token* t) { lexer_addTok(st, t); }
// static inline void parser_addErr(parser* st, const lexError* err) { lexer_addErr(st, err); }
// static inline void parser_fatalErr(parser* st, const lexError* err) { lexer_fatalErr(st, err); }

void lexer_insertBefore(parser* st, const token* t, tokenStream* point);

// remove the last token (useful for re-using standard `take*` procedures as part of others)
void lexer_delTok(parser* st);

// move an error into the warnings list
void lexer_errToWarn(parser* st, lexErrStream* err);
// static inline void parser_errToWarn(parser* st, lexErrStream* err) { lexer_errToWarn(st, err); }

#endif
