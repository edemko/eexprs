#ifndef PARAMETERS_H
#define PARAMETERS_H

#include <stdbool.h>

#include "types.h"
#include "shim/strstuff.h"


//////////////////////////////////// Numbers ////////////////////////

typedef struct radixParams {
  uint8_t radix;
  uchar* leaderLetters;
  // list of lists of digits; each list is exactly radix characters long
  // the entire list-of-lists must be terminated with a final UCHAR_NULL
  // this way, we can use `ucharElem`, but also know exactly what weight each digit carries
  // it may require duplicating digits, but that's fine
  uchar* digits;
  // base-sepcific exponent notation retains the base from the significand/mantissa
  // also exponent notation can be accessed with `qwerty^asdf` with qwerty in any base and asdf also in any base (default 10 for both)
  uchar* exponentLetters;
} radixParams;

extern const radixParams radices[]; // terminated with a `.radix == 0` entriy
extern const radixParams* defaultRadix;

// TODO bases (leader letters, exponent letters)
bool isDigit(const radixParams* base, uchar c);
bool isSign(uchar c);

extern const uchar digitSep;
extern const uchar digitPoint;

extern const uchar genericExpLetter;

// return which radixParams is named by the passed character
// return NULL if no radixParams is so named
const radixParams* decodeRadix(uchar c);

uint8_t decodeDigit(const radixParams* radix, uchar c);


//////////////////////////////////// Symbols ////////////////////////

bool isSymbolChar(uchar c);
// if there is only one char left in the stream, pass `-1` as the second arg
bool isSymbolStart(uchar cs[2]);


//////////////////////////////////// Strings ////////////////////////

bool isCodepointDelim(uchar c);
bool isStringDelim(uchar c);

strSpliceType spliceType(uchar open, uchar close);
extern uchar plainStringDelim;

bool isStringChar(uchar c);

extern uchar escapeLeader;

struct stdEscape {
  uchar source;
  uchar decode;
};
// ends with a `struct stdEscape { UCHAR_NULL, <don't care> }`.
extern struct stdEscape commonEscapes[];
extern uchar nullEscape;

extern uchar twoHexEscapeLeader;
extern uchar fourHexEscapeLeader;
extern uchar sixHexEscapeLeader;

//////////////////////////////////// Whitespace ////////////////////////

bool isSpaceChar(uchar c);
bool isNewlineChar(uchar c);

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
  All newline characters recognized are single-byte, but I pass uchar anyway to take advantage of my fancy unicode decoding.
*/
newlineType decodeNewline(uchar c[2]);
// return length in bytes of the passed type of newline
size_t newlineSize(newlineType nl);
// return the byte sequence that encodes the passed type of newline
const char* encodeNewline(newlineType nl);
/*
Compute how many bytes and unicode codepoints it is until the end of the line.
Although it cuold be constructed from `decodeNewline`, this will be much more efficient.
Also stops at utf8 decoding errors.
*/
struct untilEol { size_t bytes, uchars; };
struct untilEol untilEol(str in);


//////////////////////////////////// Punctuation ////////////////////////

bool isWrapChar(uchar c);
uchar openWrapper(uchar c);

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
  size_t uchars;
} splitter;
splitter decodeSplitter(uchar c[2]);


//////////////////////////////////// Miscellaneous ////////////////////////

extern const uchar commentChar;


#endif
