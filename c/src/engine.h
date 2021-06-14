#ifndef ENGINE_H
#define ENGINE_H

#include "types.h"
#include "parameters.h"

#define TYPE token
#include "shim/dllist.h"

#define TYPE error
#include "shim/dllist.h"


//////////////////////////////////// Lexer State ////////////////////////////////////

typedef struct openWrap {
  eexpr_wrapType type;
  eexpr_loc loc;
} openWrap;

#define TYPE openWrap
#include "shim/dynarr.h"

typedef struct engine {
  str rest; // alias into .allInput
  struct eexpr_locPoint loc; // use zero-indexed line/col and only translate to 1-indexd for human consumption
  dllist_token tokStream; //owned
  dynarr_eexpr_p eexprStream; //owned
  dllist_error warnStream; // owned
  dllist_error errStream; // owned
  error fatal; // use EEXPRERR_NOERROR for no error
  newlineType discoveredNewline; // NEWLINE_NONE if not set
  struct lexer_indent {
    bool knownMixed;
    uchar chr;
    eexpr_loc established;
  } indent;
  dynarr_openWrap wrapStack;
  str allInput; // owned
  struct lineIndex {
    size_t cap;
    size_t len;
    size_t* offsets;
  } lineIndex;
} engine;

//////////////////////////////////// Functions ////////////////////////////////////

/*
Initialize a lexer state from a file.
On error, returned `lexer.rest.bytes` is `NULL`.
Ownership of `filename` is borrowed.
*/
engine engine_newFromFile(const char* filename);

// Initialize from a sized string.
engine engine_newFromStrn(size_t n, uint8_t* input);


// free all internal data structures of the passed engine
void engine_deinit(engine* st);

void engine_rawLex(engine* st);
void engine_cookLex(engine* st);
void engine_parse(engine* st);


#endif
