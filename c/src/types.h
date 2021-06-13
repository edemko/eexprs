#ifndef TYPES_H
#define TYPES_H

#include "shim/bigint.h"
#include "shim/strstuff.h"


//////////////////////////////////// Locations ////////////////////////

typedef struct filelocPoint {
  size_t line;
  size_t col;
} filelocPoint;

typedef struct fileloc {
  filelocPoint start;
  filelocPoint end;
} fileloc;


//////////////////////////////////// Forward Declarations ////////////////////////

typedef struct eexpr eexpr;


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

typedef uchar eexprCodepoint;

typedef struct strTemplPart {
  eexpr* expr;
  str textAfter;
} strTemplPart;
#define TYPE strTemplPart
#include "shim/dynarr.h"
typedef struct eexprStrTempl {
  str text1;
  dynarr(strTemplPart) parts;
} eexprStrTempl;

// these are just so dynarr can be given a type identifier
typedef eexpr* eexprPtr;
#define TYPE eexprPtr
#include "shim/dynarr.h"
// typedef (eexpr*[2]) eexprPair;


//////////////////////////////////// Eexprs ////////////////////////

struct eexpr {
  fileloc loc;
  enum eexprType {
    EEXPR_SYMBOL,
    EEXPR_NUMBER,
    EEXPR_CODEPOINT,
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
    eexprCodepoint codepoint;
    eexprStrTempl string;
    eexpr* wrap; // paren, bracket, brace, predot
    dynarr(eexprPtr) list; // chain, space, comma, semicolon, block
    eexpr* pair[2]; // non-nullable pointers
    eexpr* ellipsis[2]; // nullable pointers
  } as;
};

void eexpr_deinit(eexpr* expr);


//////////////////////////////////// Tokens ////////////////////////

typedef enum wrapType {
  WRAP_NULL,
  WRAP_PAREN,
  WRAP_BRACK,
  WRAP_BRACE,
  WRAP_BLOCK
} wrapType;

typedef enum strSpliceType {
  STRSPLICE_PLAIN,
  STRSPLICE_OPEN,
  STRSPLICE_MIDDLE,
  STRSPLICE_CLOSE,
  STRSPLICE_CORRUPT
} strSpliceType;

typedef struct token {
  fileloc loc;
  enum tokenType {
    TOK_NUMBER,
    TOK_CODEPOINT,
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
      uchar chr;
      size_t size;
    } unknownSpace;
    eexprNumber number;
    eexprCodepoint codepoint;
    struct token_string {
      str text; // owned
      strSpliceType splice;
    } string;
    eexprSymbol symbol;
    struct token_wrap {
      wrapType type;
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

typedef struct eexprError {
  fileloc loc;
  enum eexprErrorType {
    EEXPRERR_NOERROR, // only for use as a sentinel
    EEXPRERR_BAD_BYTES,
    EEXPRERR_BAD_CHAR,
    EEXPRERR_MIXED_SPACE,
    EEXPRERR_MIXED_NEWLINES,
    EEXPRERR_BAD_DIGIT_SEPARATOR,
    EEXPRERR_MISSING_EXPONENT,
    EEXPRERR_BAD_EXPONENT_SIGN,
    EEXPRERR_BAD_CODEPOINT, // empty or badly-escaped codepoint
    EEXPRERR_BAD_ESCAPE_CHAR,
    EEXPRERR_BAD_ESCAPE_CODE,
    EEXPRERR_UNICODE_OVERFLOW,
    EEXPRERR_UNCLOSED_CODEPOINT,
    EEXPRERR_BAD_STRING_CHAR,
    EEXPRERR_MISSING_LINE_PICKUP,
    EEXPRERR_UNCLOSED_STRING,
    EEXPRERR_HEREDOC_BAD_OPEN,
    EEXPRERR_HEREDOC_BAD_INDENT_DEFINITION,
    EEXPRERR_HEREDOC_BAD_INDENTATION,
    EEXPRERR_UNCLOSED_HEREDOC,
    EEXPRERR_MIXED_INDENTATION,
    // context-sensitive errors
    EEXPRERR_TRAILING_SPACE,
    EEXPRERR_NO_TRAILING_NEWLINE,
    EEXPRERR_SHALLOW_INDENT,
    EEXPRERR_OFFSIDES,
    EEXPRERR_BAD_DOT,
    EEXPRERR_CRAMMED_TOKENS,
    // parser errors
    EEXPRERR_UNBALANCED_WRAP,
    EEXPRERR_EXPECTING_NEWLINE_OR_DEDENT,
    EEXPRERR_MISSING_TEMPLATE_EXPR,
    EEXPRERR_MISSING_CLOSE_TEMPLATE,
    EEXPRERR_EXPECT_CHAIN_AFTER_SPACE
  } type;
  union errorData {
    uchar badChar;
    uchar badCodepoint;
    uchar badEscapeChar;
    uchar badEscapeCode[6]; // if <6 uchars, then pad at start with UCHAR_NULL
    uchar unicodeOverflow;
    uchar badStringChar;
    struct eexprError_mixedIndentation {
      uchar chr;
      fileloc loc;
    } mixedIndentation;
    struct eexprError_unbalancedWrap {
      wrapType type; // what close wrap was left open, or WRAP_NULL for start-of-file
      fileloc loc; // location where the unmatched open wrap is
    } unbalancedWrap;
  } as;
} eexprError;


#endif
