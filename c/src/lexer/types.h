#ifndef LEXER_TYPES_H
#define LEXER_TYPES_H

#include "location.h"
#include "shim/bigint.h"
#include "strstuff.h"


typedef enum tokenType {
  TOK_NUMBER,
  TOK_CODEPOINT,
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
  STRSPLICE_CORRUPT
} strSpliceType;

typedef enum lexErrorType {
  LEXERR_NOERROR, // only for use as a sentinel
  LEXERR_BAD_BYTES,
  LEXERR_BAD_CHAR,
  LEXERR_MIXED_SPACE,
  LEXERR_MIXED_NEWLINES,
  LEXERR_BAD_CODEPOINT, // empty or badly-escaped codepoint
  LEXERR_MISSING_FRACTIONAL_PART,
  LEXERR_BAD_DIGIT_SEPARATOR,
  LEXERROR_MISSING_EXPONENT,
  LEXERROR_BAD_EXPONENT_SIGN,
  LEXERR_BAD_ESCAPE_CHAR,
  LEXERR_BAD_ESCAPE_CODE,
  LEXERR_UNICODE_OVERFLOW,
  LEXERR_UNCLOSED_CODEPOINT,
  LEXERR_BAD_STRING_CHAR,
  LEXERR_MISSING_LINE_PICKUP,
  LEXERR_UNCLOSED_STRING
} lexErrorType;
typedef struct lexError {
  fileloc loc;
  lexErrorType type;
  union errorData {
    uchar badChar;
    uchar badCodepoint;
    uchar badEscapeChar;
    uchar badEscapeCode[6]; // if <6 uchars, then pad at start with UCHAR_NULL
    uchar unicodeOverflow;
    uchar badStringChar;
  } as;
} lexError;

typedef struct token {
  fileloc loc;
  tokenType type;
  union tokenData {
    struct token_space {
      uchar chr;
    } space;
    struct token_number {
      bigint mantissa;
      uint8_t radix;
      uint32_t fractionalDigits;
      bigint exponent;
    } number;
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
  } as;
} token;


#endif
