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

typedef struct strTemplPart {
  eexpr* expr;
  str textAfter;
} strTemplPart;
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
  enum eexprType {
    EEXPR_SYMBOL,
    EEXPR_NUMBER,
    EEXPR_STRING,
    EEXPR_PAREN,
    EEXPR_BRACK,
    EEXPR_BRACE,
    EEXPR_BLOCK,
    EEXPR_PREDOT,
    EEXPR_CHAIN,
    EEXPR_SPACE,
    EEXPR_ELLIPSIS,
    EEXPR_COLON,
    EEXPR_COMMA,
    EEXPR_SEMICOLON
  } type;
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

// FIXME this doesn't free expr, only things it pionts to
// I should make an eexpr_del, which I think will see more use
void eexpr_deinit(eexpr* expr);


//////////////////////////////////// Tokens ////////////////////////

typedef enum strSpliceType {
  STRSPLICE_PLAIN,
  STRSPLICE_OPEN,
  STRSPLICE_MIDDLE,
  STRSPLICE_CLOSE,
  STRSPLICE_CORRUPT
} strSpliceType;

typedef struct token {
  eexpr_loc loc;
  enum tokenType {
    TOK_NUMBER,
    TOK_STRING,
    TOK_SYMBOL,
    TOK_WRAP,
    TOK_COLON,
    TOK_ELLIPSIS,
    TOK_CHAIN,
    TOK_PREDOT,
    TOK_SEMICOLON,
    TOK_COMMA,
    TOK_NEWLINE,
    TOK_SPACE,
    // tokens that will be dropped before parsing
    TOK_EOF,
    TOK_COMMENT,
    TOK_OPEN_INDENT,
    // tokens that must later be resolved in context
    TOK_UNKNOWN_SPACE,
    TOK_UNKNOWN_NEWLINE,
    TOK_UNKNOWN_COLON,
    TOK_UNKNOWN_DOT,
    // tokens that are placeholders for bad syntax (to easy colorizing)
    TOK_NUMBER_ERROR,
    TOK_STRING_ERROR,
    // a sentinel token that doesn't make it into the token stream at all
    TOK_NONE
  } type;
  union tokenData {
    struct token_unknownSpace {
      char32_t chr; // '\0' for mixed spaces, otherwise one of ' ', '\t' // TODO but I should probably make this an enum
      size_t size;
    } unknownSpace;
    eexprNumber number;
    struct token_string {
      str text; // owned
      strSpliceType splice;
    } string;
    eexprSymbol symbol;
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
} token;

void token_deinit(token* tok);


//////////////////////////////////// Errors ////////////////////////


typedef struct error {
  eexpr_loc loc;
  eexpr_errorType type;
  union errorData {
    char32_t badChar; // non-null
    char32_t badEscapeChar; // non-null
    char32_t badEscapeCode[6]; // if <6 uchars needed fo rthe digits, pad at start with '0'
    uint32_t unicodeOverflow;
    char32_t badStringChar; // non-null
    struct {
      char32_t chr; // non-null
      eexpr_loc loc;
    } mixedIndentation;
    struct {
      eexpr_wrapType type; // what close wrap was left open, or WRAP_NULL for start-of-file
      eexpr_loc loc; // location where the unmatched open wrap is
    } unbalancedWrap;
  } as;
} error;


#endif
