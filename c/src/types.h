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


//////////////////////////////////// Payloads ////////////////////////

typedef struct eexprNumber {
  bigint mantissa; // owned
  uint8_t radix;
  uint32_t fractionalDigits;
  bigint exponent; // owned
} eexprNumber;


//////////////////////////////////// Eexprs ////////////////////////

typedef struct eexpr {
  fileloc loc;
  enum eexprType {
    EEXPR_NUMBER
    // TODO
  } type;
  union eexprData {
    eexprNumber number;
  } as;
} eexpr;

void eexpr_deinit();


//////////////////////////////////// Tokens ////////////////////////

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
    TOK_WRAPPER,
    TOK_COLON,
    TOK_ELLIPSIS,
    TOK_CHAIN,
    TOK_SYNTHFIX,
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
    struct token_codepoint {
      uchar chr;
    } codepoint;
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
    struct token_indent {
      size_t depth;
    } indent; // for both open-indent and indentation
  } as;
  // some tokens can be dropped for the purposes of context-sensitive lexing and parsing
  // however, for the purposes of outputing colorization data, they should not actually be removed from the token stream.
  // `.transparent` allows these tokens to be flagged so that further lexing/parsing steps ignore them
  bool transparent;
} token;


//////////////////////////////////// Errors ////////////////////////

typedef struct lexError {
  fileloc loc;
  enum lexErrorType {
    LEXERR_NOERROR, // only for use as a sentinel
    LEXERR_BAD_BYTES,
    LEXERR_BAD_CHAR,
    LEXERR_MIXED_SPACE,
    LEXERR_MIXED_NEWLINES,
    LEXERR_MISSING_FRACTIONAL_PART,
    LEXERR_BAD_DIGIT_SEPARATOR,
    LEXERR_MISSING_EXPONENT,
    LEXERR_BAD_EXPONENT_SIGN,
    LEXERR_BAD_CODEPOINT, // empty or badly-escaped codepoint
    LEXERR_BAD_ESCAPE_CHAR,
    LEXERR_BAD_ESCAPE_CODE,
    LEXERR_UNICODE_OVERFLOW,
    LEXERR_UNCLOSED_CODEPOINT,
    LEXERR_BAD_STRING_CHAR,
    LEXERR_MISSING_LINE_PICKUP,
    LEXERR_UNCLOSED_STRING,
    LEXERR_HEREDOC_BAD_OPEN,
    LEXERR_HEREDOC_BAD_INDENT_DEFINITION,
    LEXERR_HEREDOC_BAD_INDENTATION,
    LEXERR_UNCLOSED_HEREDOC,
    LEXERR_MIXED_INDENTATION,
    // context-sensitive errors
    LEXERR_TRAILING_SPACE,
    LEXERR_NO_TRAILING_NEWLINE,
    LEXERR_SHALLOW_INDENT,
    LEXERR_OFFSIDES,
    LEXERR_BAD_DOT,
    LEXERR_CRAMMED_TOKENS
  } type;
  union errorData {
    uchar badChar;
    uchar badCodepoint;
    uchar badEscapeChar;
    uchar badEscapeCode[6]; // if <6 uchars, then pad at start with UCHAR_NULL
    uchar unicodeOverflow;
    uchar badStringChar;
    struct lexError_mixedIndentation {
      uchar chr;
      fileloc loc;
    } mixedIndentation;
  } as;
} lexError;


#endif
