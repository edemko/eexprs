#ifndef ENGINE_H
#define ENGINE_H

#include "types.h"
#include "parameters.h"

#define TYPE eexpr_token
#include "shim/dllist.h"

#define TYPE eexpr_error
#include "shim/dllist.h"


//////////////////////////////////// Lexer State ////////////////////////////////////

typedef struct openWrap {
  eexpr_wrapType type;
  eexpr_loc loc;
} openWrap;

#define TYPE openWrap
#include "shim/dynarr.h"

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

//////////////////////////////////// Functions ////////////////////////////////////

// Initialize from a sized string.
engine engine_newFromStrn(size_t n, uint8_t* input);


// free all internal data structures of the passed engine
void engine_deinit(engine* st);

void engine_rawLex(engine* st);
void engine_cookLex(engine* st);
void engine_parse(engine* st);


#endif
