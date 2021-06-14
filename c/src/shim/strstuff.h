#ifndef SHIM_STRSTUFF_H
#define SHIM_STRSTUFF_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <uchar.h>


typedef struct str {
  size_t len;
  uint8_t* bytes; // owned
} str;

/*
Reads an entire file into memory as a `str`.
If there is an error the `str.bytes` will be `NULL`; there is no further diagnosis.
The output `str.bytes` is freshly allocated with `malloc`.
*/
str readFile(const char* filename);

/*
Mallocs a cpoy of the input `str`.
*/
str str_clone(const str orig);

bool isPrefixOf(str s, str prefix);


//////////////////////////////////// String Builder ////////////////////////////////////

typedef struct strBuilder {
  size_t len;
  size_t cap;
  uint8_t* bytes; // owned
} strBuilder;

strBuilder strBuilder_new(size_t cap0);

void strBuilder_appendByte(strBuilder* self, uint8_t c);

void strBuilder_appendChar(strBuilder* self, char32_t c);

void strBuilder_append(strBuilder* self, str other);


//////////////////////////////////// Unicode Manipulation ////////////////////////////////////

/*
Single UTF-32 encoded Unicode codepoint, or error code.
Negative numbers are used for errors (usually decoding).
A decoding error is encoded as the arithmetic negative of the invalid byte.
The special value NULL_UCHAR can be used to pass "no bytes"---e.g. when an end of input has been reached.
*/

#define UCHAR_NULL ((char32_t)(-1))
_Static_assert(UCHAR_NULL < (char32_t)0 || (char32_t)0x10FFFF < UCHAR_NULL, "-1 is not a sentinel for char32_t");

/*
Read `n` unicode codepoints from `in` into the `out` array.
Return the number of bytes used from `in`.
*/
size_t peekUchars(char32_t* out, size_t n, str in);
// decode a single `char32_t` from a `str` like `peekUchars`
// returns 0 for end-of-input
size_t peekUchar(char32_t* out, str in);

/*
Check if `c` is in the array `set`.
The `set` must end in `UCHAR_NULL`.
*/
bool ucharElem(char32_t c, const char32_t* set);
// find index of c in set, undefined if `!ucharElem(c, set)`
size_t ucharFind(char32_t c, const char32_t* set);

typedef struct utf8Char {
  uint8_t nbytes;
  uint8_t codeunits[4]; // filled from the front, bytes beyond nbytes are undefined
} utf8Char;
// error conditinos have `.nbytes == 0`
utf8Char encodeUchar(char32_t c);


#endif
