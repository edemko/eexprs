#ifndef STRSTUFF_H
#define STRSTUFF_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>


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

//////////////////////////////////// Unicode Manipulation ////////////////////////////////////

/*
Single UTF-32 encoded Unicode codepoint, or error code.
Negative numbers are used for errors (usually decoding).
A decoding error is encoded as the arithmetic negative of the invalid byte.
The special value NULL_UCHAR can be used to pass "no bytes"---e.g. when an end of input has been reached.
*/
typedef int32_t uchar;
#define UCHAR_NULL INT32_MIN
#define UCHAR_SENTINEL UINT32_MAX // chosen because this is not an error, but will also never be the result of decoding; only use as part of this source code e.g. to separate one-after-another arrays of uchars

/*
Read `n` unicode codepoints from `in` into the `out` array.
Return the number of bytes used from `in`.
*/
size_t peekUchars(uchar* out, size_t n, str in);
// decode a single `uchar` from a `str` like `peekUchars`
size_t peekUchar(uchar* out, str in);

/*
Check if `c` is in the array `set`.
The `set` must end in `NULL_UCHAR`.
*/
bool ucharElem(uchar c, const uchar* set);
// find index of c in set, undefined if `!ucharElem(c, set)`
size_t ucharFind(uchar c, const uchar* set);

typedef struct utf8Char {
  uint8_t nbytes;
  uint8_t codeunits[4]; // filled from the front, bytes beyond nbytes are undefined
} utf8Char;
// error conditinos have `.nbytes == 0`
utf8Char encodeUchar(uchar c);


#endif
