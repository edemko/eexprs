/*
The most important types in this module are `token` and `lexer`.
The `lexError` type is also good to know about.
Hmmm, these types have actually moved to lexer/types.h
*/
#ifndef LEXER_H
#define LEXER_H

#include "lexer/types.h"
#include "parameters.h"



//////////////////////////////////// Lexer State ////////////////////////////////////

typedef struct tokenStream tokenStream;
struct tokenStream {
  token here; // non-null
  tokenStream* next; // owned, null for end of stream
  tokenStream* prev; // aliased, null for start of stream
};
typedef struct lexErrStream lexErrStream;
struct lexErrStream {
  lexError here; // non-null
  lexErrStream* next; // owned, null for end of stream
  lexErrStream* prev; // aliased, null for start of stream
};

typedef struct lexer {
  str rest; // alias into .allInput
  filelocPoint loc; // use zero-indexed line/col and only translate to 1-indexd for human consumption
  tokenStream* outStream; // owned, null for empty stream
  tokenStream* outStream_end; // aliased by `.outStream`, null for empty stream
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
} lexer;


//////////////////////////////////// Functions ////////////////////////////////////

/*
Initialize a lexer state from a file.
On error, returned `lexer.rest.bytes` is `NULL`.
Ownership of `filename` is borrowed.
*/
lexer lexer_newFromFile(const char* filename);

void lexer_raw(lexer* st);
void lexer_cook(lexer* st);

void lexer_advance(lexer* st, size_t bytes, size_t cols);
void lexer_incLine(lexer* st, size_t bytes);

void lexer_addTok(lexer* st, const token* t);
void lexer_addErr(lexer* st, const lexError* err);
void lexer_fatalErr(lexer* st, const lexError* err);

// remove the last token (useful for re-using standard `take*` procedures as part of others)
void lexer_delTok(lexer* st);

// move an error into the warnings list
void lexer_errToWarn(lexer* st, lexErrStream* err);

#endif
