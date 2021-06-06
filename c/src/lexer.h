/*
The most important types in this module are `token` and `lexer`.
The `lexError` type is also good to know about.
*/
#ifndef LEXER_H
#define LEXER_H

#include "location.h"
#include "parameters.h"
#include "strstuff.h"


//////////////////////////////////// Tokens ////////////////////////////////////

typedef enum tokenType {
  TOK_STRING,
  TOK_SYMBOL,
  TOK_WRAPPER,
  TOK_COLON,
  TOK_ELLIPSIS,
  TOK_CHAIN,
  TOK_FAKEFIX,
  TOK_SEMICOLON,
  TOK_COMMA,
  // tokens that will be dropped before parsing
  TOK_COMMENT,
  // tokens that must later be resolved in context
  TOK_UNKNOWN_SPACE,
  TOK_UNKNOWN_NEWLINE,
  TOK_UNKNOWN_COLON,
  TOK_UNKNOWN_DOT,
  // a sentinel token that doesn't make it into the token stream at all
  TOK_NONE
} tokenType;

typedef enum strSpliceType {
  STRSPLICE_PLAIN,
  STRSPLICE_OPEN,
  STRSPLICE_MIDDLE,
  STRSPLICE_CLOSE,
} strSpliceType;

typedef enum lexErrorType {
  LEXERR_BAD_BYTES,
  LEXERR_BAD_CHAR,
  LEXERR_MIXED_SPACE,
  LEXERR_MIXED_NEWLINES
} lexErrorType;
typedef struct lexError {
  fileloc loc;
  lexErrorType type;
  union errorData {
    struct lexerr_badChar {
      uchar chr;
    } badChar;
  } as;
} lexError;

typedef struct token {
  fileloc loc;
  tokenType type;
  union tokenData {
    struct token_space {
      uchar chr;
    } space;
    struct token_string {
      str text; // owned
      strSpliceType splice;
    } string;
    struct token_symbol {
      str text; // owned
    } symbol;
    struct token_wrapper {
      uchar chr;
      bool isOpen;
    } wrapper;
  } as;
} token;


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
  str rest; // alias into .startOfInput
  filelocPoint loc; // use zero-indexed line/col and only translate to 1-indexd for human consumption
  tokenStream* outStream; // owned, null for empty stream
  tokenStream* outStream_end; // aliased by `.outStream`, null for empty stream
  lexErrStream* errStream; // owned, null for empty stream
  lexErrStream* errStream_end; // aliased by `.errStream`, null for empty stream
  lexError* fatal; // owned, null for no fatal error
  uint8_t* startOfInput; // owned
  struct lineIndex {
    size_t cap;
    size_t len;
    size_t* offsets;
  } lineIndex;
  newlineType discoveredNewline; // NEWLINE_NONE if not set
} lexer;


//////////////////////////////////// Functions ////////////////////////////////////

/*
Initialize a lexer state from a file.
On error, returned `lexer.rest.bytes` is `NULL`.
Ownership of `filename` is borrowed.
*/
lexer lexer_newFromFile(const char* filename);
void lexer_raw(lexer* st);


void lexer_advance(lexer* st, size_t bytes, size_t cols);
void lexer_incLine(lexer* st, size_t bytes);

void lexer_addTok(lexer* st, const token* t);
void lexer_addErr(lexer* st, const lexError* err);


#endif
