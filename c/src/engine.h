#ifndef ENGINE_H
#define ENGINE_H

#include "types.h"
#include "parameters.h"

#define TYPE token
#include "shim/dllist.h"

#define TYPE eexprError
#include "shim/dllist.h"


//////////////////////////////////// Lexer State ////////////////////////////////////

typedef struct openWrap {
  wrapType type;
  fileloc loc;
} openWrap;

#define TYPE openWrap
#include "shim/dynarr.h"

typedef struct parser {
  str rest; // alias into .allInput
  filelocPoint loc; // use zero-indexed line/col and only translate to 1-indexd for human consumption
  dllist(token) tokStream; //owned
  dynarr(eexprPtr) eexprStream; //owned
  dllist(eexprError) warnStream; // owned
  dllist(eexprError) errStream; // owned
  eexprError fatal; // use EEXPRERR_NOERROR for no error
  newlineType discoveredNewline; // NEWLINE_NONE if not set
  struct lexer_indent {
    bool knownMixed;
    uchar chr;
    fileloc established;
  } indent;
  dynarr(openWrap) wrapStack;
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


#endif
