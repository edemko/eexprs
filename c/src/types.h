#ifndef TYPES_H
#define TYPES_H

#include "eexpr.h"

#include "shim/bigint.h"
#include "shim/strstuff.h"


//////////////////////////////////// Payloads ////////////////////////

typedef struct eexprSymbol {
  str text; // owned
} eexprSymbol;

typedef struct eexprNumber {
  bigint mantissa; // owned
  uint8_t radix;
  uint32_t fractionalDigits;
  bigint exponent; // owned
} eexprNumber;

typedef struct eexpr_strTemplate strTemplPart;
#define TYPE strTemplPart
#include "shim/dynarr.h"
typedef struct eexprStrTempl {
  str text1;
  dynarr_strTemplPart parts;
} eexprStrTempl;

// eexpr_p is just so dynarr can be given a type identifier
typedef eexpr* eexpr_p;
#define TYPE eexpr_p
#include "shim/dynarr.h"


//////////////////////////////////// Eexprs ////////////////////////

struct eexpr {
  eexpr_loc loc;
  eexpr_type type;
  union eexprData {
    eexprSymbol symbol;
    eexprNumber number;
    eexprStrTempl string;
    eexpr* wrap; // paren, bracket, brace, predot
    dynarr_eexpr_p list; // chain, space, comma, semicolon, block
    eexpr* pair[2]; // non-nullable pointers
    eexpr* ellipsis[2]; // nullable pointers
  } as;
};


//////////////////////////////////// Tokens ////////////////////////

struct eexpr_token {
  eexpr_loc loc;
  eexpr_tokenType type;
  union tokenData {
    eexprSymbol symbol;
    struct token_unknownSpace {
      eexpr_spaceType type;
      size_t size;
    } unknownSpace;
    eexprNumber number;
    struct token_string {
      str text; // owned
      eexpr_stringType splice;
    } string;
    struct token_wrap {
      eexpr_wrapType type;
      bool isOpen;
    } wrap;
    struct token_indent {
      size_t depth;
    } indent; // for both open-indent and indentation
  } as;
  // some tokens can be dropped for the purposes of context-sensitive lexing and parsing
  // however, for the purposes of outputing colorization data, they should not actually be removed from the token stream.
  // `.transparent` allows these tokens to be flagged so that further lexing/parsing steps ignore them
  bool transparent;
};

void token_deinit(eexpr_token* tok);


#endif
