#ifndef INTERNAL_PARAMETERS_H
#define INTERNAL_PARAMETERS_H

#include <stdbool.h>

#include "types.h"
#include "strstuff.h"


//////////////////////////////////// Numbers ////////////////////////

typedef struct radixParams {
  uint8_t radix;
  char32_t* leaderLetters;
  // list of lists of digits; each list is exactly radix characters long
  // the entire list-of-lists must be terminated with a final UCHAR_NULL
  // this way, we can use `ucharElem`, but also know exactly what weight each digit carries
  // it may require duplicating digits, but that's fine
  char32_t* digits;
  // base-sepcific exponent notation retains the base from the significand/mantissa
  // also exponent notation can be accessed with `qwerty^asdf` with qwerty in any base and asdf also in any base (default 10 for both)
  char32_t* exponentLetters;
} radixParams;

extern const radixParams radices[]; // terminated with a `.radix == 0` entriy
extern const radixParams* defaultRadix;

bool isDigit(const radixParams* base, char32_t c);

extern const char32_t positiveSign;
extern const char32_t negativeSign;
bool isSign(char32_t c);

extern const char32_t digitSep;
extern const char32_t digitPoint;

extern const char32_t genericExpLetter;

// return which radixParams is named by the passed character
// return NULL if no radixParams is so named
const radixParams* decodeRadix(char32_t c);

uint8_t decodeDigit(const radixParams* radix, char32_t c);


//////////////////////////////////// Symbols ////////////////////////

bool isSymbolChar(char32_t c);
// if there is only one char left in the stream, pass `-1` as the second arg
bool isSymbolStart(char32_t cs[2]);


//////////////////////////////////// Strings ////////////////////////

bool isCodepointDelim(char32_t c);
bool isStringDelim(char32_t c);

eexpr_stringType spliceType(char32_t open, char32_t close);
extern char32_t plainStringDelim;
extern char32_t sqlStringDelim;

bool isStringChar(char32_t c);

extern char32_t escapeLeader;

struct stdEscape {
  char32_t source;
  char32_t decode;
};
// ends with a `struct stdEscape { UCHAR_NULL, <don't care> }`.
extern struct stdEscape commonEscapes[];
extern char32_t nullEscape;

extern char32_t twoHexEscapeLeader;
extern char32_t fourHexEscapeLeader;
extern char32_t sixHexEscapeLeader;

//////////////////////////////////// Whitespace ////////////////////////

extern const char32_t spaceChar;
extern const char32_t tabChar;
bool isSpaceChar(char32_t c);
bool isNewlineChar(char32_t c);

eexpr_spaceType decodeSpaceChar(char32_t c);


typedef enum newlineType {
  NEWLINE_NONE,
  NEWLINE_UNIX,
  NEWLINE_WINDOWS,
  NEWLINE_C64,
  NEWLINE_SPOOLED,
  NEWLINE_QNX
} newlineType;
/*
  Return the type of a newline that the input starts with.
  If the input does not start with a newline, returns NEWLINE_NONE.
  All newline characters recognized are single-byte, but I pass char32_t anyway to take advantage of my fancy unicode decoding.
*/
newlineType decodeNewline(char32_t c[2]);
// return length in bytes of the passed type of newline
size_t newlineSize(newlineType nl);
// return the byte sequence that encodes the passed type of newline
const char* encodeNewline(newlineType nl);
/*
Compute how many bytes and unicode codepoints it is until the end of the line.
Although it cuold be constructed from `decodeNewline`, this will be much more efficient.
Also stops at utf8 decoding errors.
*/
struct untilEol { size_t bytes, chars; };
struct untilEol untilEol(str in);


//////////////////////////////////// Punctuation ////////////////////////

eexpr_wrapType isWrapChar(char32_t c);
bool isOpenWrap(char32_t c);

typedef enum splitterType {
  SPLITTER_NONE,
  SPLITTER_COLON,
  SPLITTER_ELLIPSIS,
  SPLITTER_DOT,
  SPLITTER_SEMICOLON,
  SPLITTER_COMMA
} splitterType;
typedef struct splitter {
  splitterType type;
  size_t bytes;
  size_t chars;
} splitter;
splitter decodeSplitter(char32_t c[2]);


//////////////////////////////////// Miscellaneous ////////////////////////

extern const char32_t commentChar;


#endif
