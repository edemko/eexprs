#include <assert.h>
#include <string.h>

#include "lexer/util.h"
#include "parameters.h"
#include "shim/common.h"


//////////////////////////////////// Helper Consumers ////////////////////////////////////


// decode a hex-encoded unicode codepoint into `out`
// if decoding fails, return false and do not modify `out`
static
bool decodeUnihex(char32_t* out, size_t nDigits, char32_t* digits) {
  char32_t accum = 0;
  for (int i = 0; i < nDigits; ++i) {
    accum = accum << 4;
    if ('0' <= digits[i] && digits[i] <= '9') {
      accum |= digits[i] - '0';
    }
    else if ('a' <= digits[i] && digits[i] <= 'f') {
      accum |= digits[i] - 'a' + 10;
    }
    else if ('A' <= digits[i] && digits[i] <= 'F') {
      accum |= digits[i] - 'A' + 10;
    }
    else {
      return false;
    }
  }
  *out = accum;
  return true;
}

/* Char escapes are escape sequences which are interpreted as exactly one codepoint.
  They can be one of the "common" escape characters, or a as a 2-6 byte hexadecimal escape.
  `\\[:commonEscapeChar:]`
  `\\x[:hexDigit:]{2}`
  `\\u[:hexDigit:]{4}`
  `\\U[:hexDigit:]{6}`
*/
// helper for takeCodepoint and takeString
// returns a single char32_t, or UCHAR_NULL if no valid escape sequence is found
// input is consumed and errors are emitted (escpe no error is emitted if no valid escape is found; allows chaining with takeStrEscape)
// call only after detecting an escapeLeader
static
char32_t takeCharEscape(engine* st) {
  char32_t c;
  size_t adv;
  adv = peekUchar(&c, st->rest);
  // standard escapes
  for (size_t i = 0; commonEscapes[i].source != UCHAR_NULL; ++i) {
    if (c == commonEscapes[i].source) {
      lexer_advance(st, adv, 1);
      return commonEscapes[i].decode;
    }
  }
  char32_t digits[6] = {'0', '0', '0', '0', '0', '0'};
  eexpr_error decodeError = {.loc = {.start = st->loc}, .type = EEXPRERR_BAD_ESCAPE_CODE};
  if (c == twoHexEscapeLeader) {
    lexer_advance(st, adv, 1);
    adv = peekUchars(&digits[4], 2, st->rest);
    lexer_advance(st, adv, 2);
    if (!decodeUnihex(&c, 2, &digits[4])) {
      decodeError.loc.end = st->loc;
      for (int i = 0; i < 6; ++i) { decodeError.as.badEscapeCode[i] = digits[i]; }
      dllist_insertAfter_eexpr_error(&st->errStream, NULL, &decodeError);
    }
    return c;
  }
  else if (c == fourHexEscapeLeader) {
    lexer_advance(st, adv, 1);
    adv = peekUchars(&digits[2], 4, st->rest);
    lexer_advance(st, adv, 4);
    if (!decodeUnihex(&c, 4, &digits[2])) {
      decodeError.loc.end = st->loc;
      for (int i = 0; i < 6; ++i) { decodeError.as.badEscapeCode[i] = digits[i]; };
      dllist_insertAfter_eexpr_error(&st->errStream, NULL, &decodeError);
    }
    return c;
  }
  else if (c == sixHexEscapeLeader) {
    lexer_advance(st, adv, 1);
    adv = peekUchars(digits, 6, st->rest);
    lexer_advance(st, adv, 6);
    if (!decodeUnihex(&c, 6, digits)) {
      decodeError.loc.end = st->loc;
      for (int i = 0; i < 6; ++i) { decodeError.as.badEscapeCode[i] = digits[i]; };
      dllist_insertAfter_eexpr_error(&st->errStream, NULL, &decodeError);
    }
    return c;
  }
  else {
    return UCHAR_NULL;
  }
}

/*
Null escapes may appear in strings and are interpreted as empty strings.
One is inspired by Haskell (which has no fixed-length hex escape sequences),
  and really only reserved as a null escape just in case it is needed for any future extensions.
The other is used for breaking strings across lines.
  `\\&`
  `\\[:linebreak:][:whitespace:]*\\`

Comments/whitespace are not allowed before the first linebreak to keep lookahead constant-space.
*/
// As `takeCharEscape`, but returns true iff characters were consumed.
static bool takeNewline(engine* st);
static bool takeWhitespace(engine* st);
static
bool takeNullEscape(engine* st) {
  char32_t c;
  size_t adv;
  adv = peekUchar(&c, st->rest);
  if (isNewlineChar(c)) {
    assert(takeNewline(st));
    lexer_delTok(st);
    if (takeWhitespace(st)) { lexer_delTok(st); }
    adv = peekUchar(&c, st->rest);
    if (c == escapeLeader) {
      lexer_advance(st, adv, 1);
    }
    else {
      eexpr_error err = {.loc = {.start = st->loc, .end = st->loc}, .type = EEXPRERR_MISSING_LINE_PICKUP};
      dllist_insertAfter_eexpr_error(&st->errStream, NULL, &err);
    }
    return true;
  }
  else if (c == nullEscape) {
    lexer_advance(st, adv, 1);
    return true;
  }
  return false;
}

static
void tryBadBytes(engine* st, bool fatal) {
  char32_t c; size_t adv = peekUchar(&c, st->rest);
  if (c != UCHAR_NULL || adv == 0) { return; }
  eexpr_error err = {.loc = {.start = st->loc}, .type = EEXPRERR_BAD_BYTES};
  while (true) {
    adv = peekUchar(&c, st->rest);
    if (c != UCHAR_NULL || adv == 0) { break; }
    lexer_advance(st, adv, 0);
  }
  err.loc.end = st->loc;
  if (fatal) {
    st->fatal = err;
  }
  else {
    dllist_insertAfter_eexpr_error(&st->errStream, NULL, &err);
  }
}

//////////////////////////////////// Individual Token Consumers ////////////////////////////////////

/*
(Inline) whitespace is simply one or more whitespace characters of any type.
  `[:whitespaceChar:]+`
However, if the whitespace is not simply a repetition of the same character, that it a "mixed whitespace" error.
*/
static
bool takeWhitespace(engine* st) {
  {
    char32_t lookahead;
    peekUchar(&lookahead, st->rest);
    if (!isSpaceChar(lookahead)) { return false; }
  }
  eexpr_token tok = {.loc = {.start = st->loc}, .type = EEXPR_TOK_UNKNOWN_SPACE};
  {
    char32_t c0; peekUchar(&c0, st->rest);
    tok.as.unknownSpace.type = decodeSpaceChar(c0);
  }
  size_t advChars = 0;
  while (true) {
    char32_t c; size_t adv = peekUchar(&c, st->rest);
    if (isSpaceChar(c)) {
      eexpr_spaceType newWs = decodeSpaceChar(c);
      if (newWs != tok.as.unknownSpace.type) {
        tok.as.unknownSpace.type = EEXPR_WSMIXED;
      }
      lexer_advance(st, adv, 1);
      advChars += 1;
    }
    else {
      break;
    }
  }
  assert(advChars != 0);
  tok.loc.end = st->loc;
  tok.as.unknownSpace.size = advChars;
  lexer_addTok(st, &tok);
  if (tok.as.unknownSpace.type == EEXPR_WSMIXED) {
    eexpr_error err = { .loc = tok.loc, .type = EEXPRERR_MIXED_SPACE };
    dllist_insertAfter_eexpr_error(&st->errStream, NULL, &err);
  }
  return true;
}

/* Lines can be joined with a backslash+newline.
  `\\[:whitespace:]*[:newline:]`
*/
static
bool takeLineContinue(engine* st) {
  eexpr_token tok = {.loc = {.start = st->loc}, .type = EEXPR_TOK_UNKNOWN_SPACE};
  {
    char32_t lookahead;
    size_t adv = peekUchar(&lookahead, st->rest);
    if (lookahead != escapeLeader) { return false; }
    lexer_advance(st, adv, 1);
  }
  tok.as.unknownSpace.type = EEXPR_WSLINECONTINUE;
  tok.as.unknownSpace.size = 0;
  { // detect trailing whitespace
    eexpr_error err = {.loc = {.start = st->loc}, .type = EEXPRERR_TRAILING_SPACE};
    bool trailingSpace = false;
    while (true) {
      char32_t c;
      size_t adv = peekUchar(&c, st->rest);
      if (isSpaceChar(c)) {
        lexer_advance(st, adv, 1);
        trailingSpace = true;
      }
      else { break; }
    }
    if (trailingSpace) {
      err.loc.end = st->loc;
      dllist_insertAfter_eexpr_error(&st->errStream, NULL, &err);
    }
  }
  if (takeNewline(st)) {
    lexer_delTok(st);
    tok.loc.end = st->loc;
    lexer_addTok(st, &tok);
  }
  else {
    eexpr_error err = {.loc = {.start = tok.loc.start, .end = st->loc}, .type = EEXPRERR_BAD_CHAR};
    err.as.badChar = escapeLeader;
    dllist_insertAfter_eexpr_error(&st->errStream, NULL, &err);
  }
  return true;
}

/*
Newlines are one of the newline sequences (i.e. alternation of literals).
See `parameters.c` for valid newline sequences.
However, if the newlines of a file are not all the same sequence, that it a "mixed newlines" error.
*/
static
bool takeNewline(engine* st) {
  newlineType type;
  {
    char32_t lookahead[2];
    peekUchars(lookahead, 2, st->rest);
    type = decodeNewline(lookahead) != 0;
    if (type == NEWLINE_NONE) { return false; }
  }
  eexpr_token tok = {.loc = {.start = st->loc}, .type = EEXPR_TOK_UNKNOWN_NEWLINE};
  lexer_incLine(st, newlineSize(type));
  tok.loc.end = st->loc;
  lexer_addTok(st, &tok);
  if (type != st->discoveredNewline) {
    if (st->discoveredNewline == NEWLINE_NONE) {
      st->discoveredNewline = type;
    }
    else {
      eexpr_error err =
      { .loc = tok.loc
      , .type = EEXPRERR_MIXED_NEWLINES
      };
      dllist_insertAfter_eexpr_error(&st->errStream, NULL, &err);
    }
  }
  return true;
}

/*
  The end of the file also counts as a newline.
*/
static
bool takeEof(engine* st) {
  char32_t c;
  size_t adv = peekUchar(&c, st->rest);
  if (adv != 0) {
    return false;
  }
  eexpr_token tok = {.loc = {.start = st->loc, .end = st->loc}, .type=EEXPR_TOK_EOF};
  lexer_addTok(st, &tok);
  return true;
}

/*
Comments start with `#` and continue until the end-of-line (incl. end-of-file).
  `#[^:newlineChar:]*`
I have decided against block comments because either
  a) they do not nest, which is super-lame, or
  b) they do nest, in which case I would need a stack in order to perform lexing.
Neither option is appealing.
Comment tokens do not carry their contents, as they should not be used in place of proper syntax for preprocessors, pragmas, or documentation.
*/
// TODO detect trailing whitespace at end of comment
static
bool takeComment(engine* st) {
  {
    char32_t lookahead;
    peekUchar(&lookahead, st->rest);
    if (lookahead != commentChar) { return false; }
  }
  eexpr_token tok = {.loc = {.start = st->loc}, .type = EEXPR_TOK_COMMENT};
  struct untilEol skip = untilEol(st->rest);
  lexer_advance(st, skip.bytes, skip.chars);
  tok.loc.end = st->loc;
  lexer_addTok(st, &tok);
  tryBadBytes(st, true); // since untilEol will also stop at decoding errors
  return true;
}

/*
Symbols are simply one or more symbol characters.
  `[:symbolChar:]+`
*/
static
bool takeSymbol(engine* st) {
  {
    char32_t lookahead[2];
    peekUchars(lookahead, 2, st->rest);
    if (!isSymbolStart(lookahead)) { return false; }
  }
  str text = { .len = 0, .bytes = st->rest.bytes };
  eexpr_token tok = {.loc = {.start = st->loc}, .type = EEXPR_TOK_SYMBOL};
  while (true) {
    char32_t c;
    size_t adv = peekUchar(&c, st->rest);
    if (isSymbolChar(c)) {
      text.len += adv;
      lexer_advance(st, adv, 1);
    }
    else {
      break;
    }
  }
  assert(text.len != 0);
  tok.loc.end = st->loc;
  tok.as.symbol.text = str_clone(text);
  lexer_addTok(st, &tok);
  return true;
}

static
void checkDigitSepContext(const radixParams* radix, struct eexpr_locPoint start, bool alwaysError, engine* st) {
  char32_t lookahead;
  peekUchar(&lookahead, st->rest);
  if ( alwaysError
    || (!isDigit(radix, lookahead) && lookahead != digitSep)
     ) {
    eexpr_error err = {.loc = {.start = start, .end = st->loc}, .type = EEXPRERR_BAD_DIGIT_SEPARATOR};
    dllist_insertAfter_eexpr_error(&st->errStream, NULL, &err);
  }
}
/*
Numbers can be integers or fractionals; fractionals are distinguished by having a decimal point.
They start with an optional sign, then an optional radix specification (specs exist for bases 2, 8, 12, 16; default 10).
Underscores (including multiple underscores) may be used between any two digits as separators.
If the number has an exponent, it is introduced with an exponent letter (either base-specific or the generic `^`).
If the number is fractional, the exponent may have a sign (no sign is allowed for integers).
Then, if the exponent letter was generic, a radix specificaion may be given (just like before, but now it influences the exponent).
  `[+-]?(0[:radixLetter(x):])?[:digit(x):_]+([:expLetter(x):][:digit(x):_]+)?`
  `[+-]?(0[:radixLetter(x):])?[:digit(x):_]+(^(0[:radixLetter(y):])?[:digit(y):_]+)?`
  `[+-]?(0[:radixLetter(x):])?[:digit(x):_]+\.[:digit(x):_]+([:expLetter(x):][+-]?[:digit(x):_]+)?`
  `[+-]?(0[:radixLetter(x):])?[:digit(x):_]+\.[:digit(x):_]+(^[+-]?(0[:radixLetter(y):])?[:digit(y):_]+)?`
  WARNING: I've taken some liberties in these regexes about underscores.
*/
static
bool takeNumber(engine* st) {
  eexpr_token tok = {.loc = {.start = st->loc}, .type = EEXPR_TOK_NUMBER};
  ////// gather sign (or early exit) //////
  bool neg;
  {
    char32_t lookahead[2];
    size_t adv = peekUchars(lookahead, 1, st->rest);
    if (isDigit(defaultRadix, lookahead[0])) {
      neg = false;
    }
    else if (isSign(lookahead[0])) {
      neg = lookahead[0] == negativeSign;
      peekUchars(lookahead, 2, st->rest);
      if (isDigit(defaultRadix, lookahead[1])) {
        lexer_advance(st, adv, 1);
      }
      else { return false; }
    }
    else { return false; }
  }
  ////// determine radix //////
  const radixParams* radix = NULL;
  {
    char32_t lookahead[2];
    size_t adv = peekUchars(lookahead, 2, st->rest);
    if (lookahead[0] == defaultRadix->digits[0]) {
      radix = decodeRadix(lookahead[1]);
      if (radix != NULL) {
        lexer_advance(st, adv, 2);
      }
    }
    if (radix == NULL) {
      radix = defaultRadix;
    }
  }
  ////// gather integer part //////
  bigint mantissa = bigint_new();
  {
    uint32_t integerDigits = 0;
    while (true) {
      char32_t c;
      size_t adv = peekUchar(&c, st->rest);
      if (isDigit(radix, c)) {
        lexer_advance(st, adv, 1);
        bigint_scale(&mantissa, radix->radix);
        bigint_inc(&mantissa, decodeDigit(radix, c));
        integerDigits += 1;
      }
      else if (c == digitSep) {
        struct eexpr_locPoint loc0 = st->loc;
        lexer_advance(st, adv, 1);
        checkDigitSepContext(radix, loc0, integerDigits == 0, st);
      }
      else { break; }
    }
  }
  ////// gather fractional part //////
  uint32_t fractionalDigits = 0;
  { // decimal point
    char32_t lookahead[2];
    size_t adv = peekUchar(lookahead, st->rest);
    peekUchars(lookahead, 2, st->rest);
    if (lookahead[0] == digitPoint && isDigit(radix, lookahead[1])) {
      lexer_advance(st, adv, 1);
      while (true) {
        char32_t c;
        size_t adv = peekUchar(&c, st->rest);
        if (isDigit(radix, c)) {
          lexer_advance(st, adv, 1);
          bigint_scale(&mantissa, radix->radix);
          bigint_inc(&mantissa, decodeDigit(radix, c));
          fractionalDigits += 1;
        }
        else if (c == digitSep) {
          struct eexpr_locPoint loc0 = st->loc;
          lexer_advance(st, adv, 1);
          checkDigitSepContext(radix, loc0, fractionalDigits == 0, st);
        }
        else { break; }
      }
    }
  }
  ////// gather exponent //////
  bool expNeg;
  bigint exponent = bigint_new();
  {
    ////// determine presence and type of exponent //////
    bool expPresent; 
    bool expRadixMayDiffer;
    {
      char32_t lookahead;
      size_t adv = peekUchar(&lookahead, st->rest);
      if (ucharElem(lookahead, radix->exponentLetters)) {
        lexer_advance(st, adv, 1);
        expPresent = true;
        expRadixMayDiffer = false;
      }
      else if (lookahead == genericExpLetter) {
        lexer_advance(st, adv, 1);
        expPresent = true;
        expRadixMayDiffer = true;
      }
      else {
        expPresent = false;
      }
    }
    if (expPresent) {
      ////// gather exponent sign //////
      {
        char32_t lookahead;
        size_t adv = peekUchar(&lookahead, st->rest);
        if (isSign(lookahead)) {
          if (fractionalDigits) {
            expNeg = lookahead == negativeSign;
            lexer_advance(st, adv, 1);
          }
          else {
            expNeg = false;
            eexpr_error err = {.loc = {.start = st->loc}, .type = EEXPRERR_BAD_EXPONENT_SIGN};
            lexer_advance(st, adv, 1);
            err.loc.end = st->loc;
            dllist_insertAfter_eexpr_error(&st->errStream, NULL, &err);
          }
        }
        else {
          expNeg = false;
        }
      }
      ////// gather exponent radix //////
      const radixParams* expRadix = NULL;
      if (!expRadixMayDiffer) {
        expRadix = radix;
      }
      else {
        char32_t lookahead[2];
        size_t adv = peekUchars(lookahead, 2, st->rest);
        if (lookahead[0] == defaultRadix->digits[0]) {
          expRadix = decodeRadix(lookahead[1]);
          if (expRadix != NULL) {
            lexer_advance(st, adv, 2);
          }
        }
        if (expRadix == NULL) {
          expRadix = defaultRadix;
        }
      }
      ////// gather exponent digits //////
      {
        // ensure exponent has at least one digit
        uint32_t expDigits = 0;
        while (true) {
          char32_t c;
          size_t adv = peekUchar(&c, st->rest);
          if (isDigit(expRadix, c)) {
            expDigits += 1;
            lexer_advance(st, adv, 1);
            bigint_scale(&exponent, expRadix->radix);
            bigint_inc(&exponent, decodeDigit(expRadix, c));
          }
          else if (c == digitSep) {
            struct eexpr_locPoint loc0 = st->loc;
            lexer_advance(st, adv, 1);
            checkDigitSepContext(expRadix, loc0, expDigits == 0, st);
          }
          else { break; }
        }
        if (expDigits == 0) {
          tok.type = EEXPR_TOK_NUMBER_ERROR;
          tok.loc.end = st->loc;
          lexer_addTok(st, &tok);
          eexpr_error err = {.loc = tok.loc, .type = EEXPRERR_MISSING_EXPONENT};
          dllist_insertAfter_eexpr_error(&st->errStream, NULL, &err);
          return true;
        }
      }
    }
  }
  tok.loc.end = st->loc;
  if (mantissa.len != 0) { mantissa.pos = !neg; }  // finally make use of the sign we may have parsed at the beginning
  tok.as.number.mantissa = mantissa;
  tok.as.number.radix = radix->radix;
  tok.as.number.fractionalDigits = fractionalDigits;
  if (exponent.len != 0) { exponent.pos = !expNeg; }
  tok.as.number.exponent = exponent;
  lexer_addTok(st, &tok);
  return true;
}

/*
Strings are make of a number of (reasonable, as in the codepoitn parser) characters and escape sequences.
The valid escape sequences are those of character strings, plus null escape sequences (see `takeNullEscape`).
  `[:strDelimChar:]([:stringChar:]|\\[:charEscape:]|[:nullEscape:])*[:strDelimChar:]`
An entire string template is between two double-quotes, but the template may have eexprs interpolated into it between backticks.
This parser really only creates a token for each string part.
These parts can end with a backtick (to begin an interpolation), start with a backtick (to resume the string after interpolation), or both.
*/
static
bool takeString(engine* st) {
  eexpr_token tok = {.loc = {.start = st->loc}, .type = EEXPR_TOK_STRING};
  char32_t open; {
    size_t adv = peekUchar(&open, st->rest);
    if (!isStringDelim(open)) { return false; }
    lexer_advance(st, adv, 1);
  }
  strBuilder buf = strBuilder_new(128);
  for (bool more = true; more; ) {
    more = false;
    { // standard characters
      str tmp = {.len = 0, .bytes = st->rest.bytes};
      while (true) {
        char32_t c;
        size_t adv = peekUchar(&c, st->rest);
        if (!isStringChar(c)) { break; }
        lexer_advance(st, adv, 1);
        tmp.len += adv;
      }
      if (tmp.len != 0) {
        more = true;
        strBuilder_append(&buf, tmp);
      }
    }
    { // escape sequences
      char32_t c;
      size_t adv = peekUchar(&c, st->rest);
      if (c == escapeLeader) {
        lexer_advance(st, adv, 1);
        more = true;
        char32_t decoded = takeCharEscape(st);
        if (decoded != UCHAR_NULL) { // found a single-character escape
          utf8Char encoded = encodeUchar(decoded);
          str tmp = {.len = encoded.nbytes, .bytes = encoded.codeunits};
          strBuilder_append(&buf, tmp);
        }
        else if (takeNullEscape(st)) { // found a null escape
          // do nothing
        }
        else {
          adv = peekUchar(&c, st->rest);
          if (adv == 0) {
            // if it was end of file, let the next stage deal with it
          }
          else if (c == UCHAR_NULL) { // corrupt bytes
            more = true;
            tryBadBytes(st, false);
          }
          else { // no valid escape sequence found
            more = true;
            eexpr_error err = {.loc = {.start = st->loc}, .type = EEXPRERR_BAD_ESCAPE_CHAR, .as.badEscapeChar = c};
            lexer_advance(st, adv, 1);
            err.loc.end = st->loc;
            dllist_insertAfter_eexpr_error(&st->errStream, NULL, &err);
          }
        }
      }
    }
    {
      char32_t c;
      size_t adv = peekUchar(&c, st->rest);
      // stop at close delimiter or end of line/file
      if ( adv == 0
        || isStringDelim(c)
        || isNewlineChar(c)
        ) { break; }
      else if (!more) { // characters that did not match above are invalid and error recovery should skip them
        eexpr_error err = {.loc = {.start = st->loc}, .type = EEXPRERR_BAD_STRING_CHAR, .as.badStringChar = c};
        lexer_advance(st, adv, 1);
        err.loc.end = st->loc;
        dllist_insertAfter_eexpr_error(&st->errStream, NULL, &err);
      }
    }
  }
  char32_t close; {
    size_t adv = peekUchar(&close, st->rest);
    if (isStringDelim(close)) {
      lexer_advance(st, adv, 1);
    }
    else {
      eexpr_error err = {.loc = {.start = st->loc, .end = st->loc}, .type = EEXPRERR_UNCLOSED_STRING};
      dllist_insertAfter_eexpr_error(&st->errStream, NULL, &err);
    }
  }
  tok.loc.end = st->loc;
  tok.as.string.text.len = buf.len;
  tok.as.string.text.bytes = realloc(buf.bytes, buf.len);
  tok.as.string.splice = spliceType(open, close);
  lexer_addTok(st, &tok);
  return true;
}

/*
Single-quote strings offer a way to write strings with minimal escaping.
In particular, I expect it will help significatly with embedding languages that make frequent use of backslash (e.g. regex).
They are any characters enclosed in single-quotes, where single-quotes can be embedded by doubling them.
  `'([^']|'')*'`
*/
bool takeSqlString(engine* st) {
  char32_t c; size_t adv = peekUchar(&c, st->rest);
  if (c != sqlStringDelim) { return false; }
  eexpr_token tok = {.loc = {.start = st->loc}, .type = EEXPR_TOK_STRING};
  lexer_advance(st, adv, 1);
  strBuilder buf = strBuilder_new(128);
  while (true) {
    adv = peekUchar(&c, st->rest);
    str tmp = {.len = adv, .bytes = st->rest.bytes};
    if (isNewlineChar(c)) {
      if (takeNewline(st)) {
        lexer_delTok(st);
        tmp.len = st->rest.bytes - tmp.bytes;
        strBuilder_append(&buf, tmp);
      }
      else {
        free(buf.bytes);
        st->fatal.type = EEXPRERR_UNCLOSED_MULTILINE_STRING;
        st->fatal.loc.start = tok.loc.start;
        st->fatal.loc.end = st->loc;
        return true;
      }
    }
    else if (c == sqlStringDelim) {
      char32_t lookahead[2]; size_t bigAdv = peekUchars(lookahead, 2, st->rest);
      if (lookahead[1] == sqlStringDelim) {
        strBuilder_append(&buf, tmp);
        lexer_advance(st, bigAdv, 1);
      }
      else {
        lexer_advance(st, adv, 1);
        tok.loc.end = st->loc;
        tok.as.string.text.len = buf.len;
        tok.as.string.text.bytes = realloc(buf.bytes, buf.len);
        tok.as.string.splice = EEXPR_STRPLAIN;
        lexer_addTok(st, &tok);
        return true;
      }
    }
    else if (adv == 0) {
      free(buf.bytes);
      st->fatal.type = EEXPRERR_UNCLOSED_MULTILINE_STRING;
      st->fatal.loc.start = tok.loc.start;
      st->fatal.loc.end = st->loc;
      return true;
    }
    else if (c == UCHAR_NULL) {
      tryBadBytes(st, false);
    }
    else {
      lexer_advance(st, adv, 1);
      strBuilder_append(&buf, tmp);
    }
  }
}
// /*
// A codepoint literal is any (reasonable) character between single-ticks.
// Escape sequences are also permitted, as long as they encode exactly one codepoint.
//   `'[:stringChar::charEscape:]'`
// */
// // the question is, do I even want codepoint literals if I could just `fromString` them?
// // I could make eexpr consumers recognize something like `c"\n"` or `fromString` them under the appropriate type context
// // if I take them out, that would allow me to write sql strings (single-quote-delimited multiline strings where the only escape sequence---and the only one needed---is two single-quotes to insert a snigle quote into the string
// // heck, how often does a programmer want a codepoint literal instead of a grapheme cluster literal, or a user-perceived character literal?
// //   I don't see why I should favor codepoints by making them so integral to eexprs


/*
Heredocs offer a way to embed multi-line strings without escaping.
They are delimited by triple-quotes plus an optional symbol.
The heredoc only ends when the start of the next line begins with the symbol followed by triple quotes.
Heredocs can also be defined with indentation, which is done with a pair of backslashes:
  the first is a flag that signals we need to look for indentation,
  the second is placed where the indentation ends.
The total indentation will be either:
  a) the number of spaces at the start of the next line plus one (because the backslash takes a column), or
  b) the number of tabs at the start of the next line plus one.
If the indentation is listed in tabs, the terminating backslash must be followed by a tab in order to retain alignment.
  `"""([:symbolChar:]*)[:whitespace:][:newline:]([^:newline:]*[:newline:])*?\1"""`
  `"""([:symbolChar:]*)[:whitespace:]\\[:whitespace:][:newline:]( *)\\([^:newline:]*[:newline:]\2 )*?\1"""`
  `"""([:symbolChar:]*)[:whitespace:]\\[:whitespace:][:newline:](\t*)\\\t([^:newline:]*[:newline:]\2\t)*?\1"""`
The last newline of a heredoc is not included in the string.
If you want a trailing newline in the string, explicitly include a blank line.
*/
static
bool takeHeredoc(engine* st) {
  eexpr_token tok = {.loc = {.start = st->loc}, .type = EEXPR_TOK_STRING};
  {
    char32_t lookahead[3];
    size_t adv = peekUchars(lookahead, 3, st->rest);
    if ( lookahead[0] != plainStringDelim
      || lookahead[1] != plainStringDelim
      || lookahead[2] != plainStringDelim
       ) { return false; }
    lexer_advance(st, adv, 3);
    tok.as.string.splice = EEXPR_STRPLAIN;
  }
  str ender;
  size_t enderNChars = 0;
  { // accumulate delimiter name
    str delimName = {.len = 0, .bytes = st->rest.bytes};
    while (true) {
      char32_t c;
      size_t adv = peekUchar(&c, st->rest);
      if (isSymbolChar(c)) {
        delimName.len += adv;
        lexer_advance(st, adv, 1);
        enderNChars += 1;
      }
      else { break; }
    }
    size_t quoteBytes = encodeUchar(plainStringDelim).nbytes;
    ender.len = delimName.len + 3*quoteBytes;
    ender.bytes = malloc(ender.len * sizeof(uint8_t));
    checkOom(ender.bytes);
    memcpy(ender.bytes, delimName.bytes, delimName.len);
    ender.bytes[ender.len - 3*quoteBytes]
      = ender.bytes[ender.len - 2*quoteBytes]
      = ender.bytes[ender.len - 1*quoteBytes]
      = plainStringDelim;
    enderNChars += 3;
  }
  bool indented = false;
  { // detect indentation flag (skipping whitespace around first backslash)
    eexpr_error err = {.loc = {.start = st->loc}, .type = EEXPRERR_TRAILING_SPACE};
    bool trailingSpace = false;
    while (true) {
      char32_t c; size_t adv = peekUchar(&c, st->rest);
      if (isSpaceChar(c)) {
        lexer_advance(st, adv, 1);
        trailingSpace = true;
      }
      else { break; }
    }
    char32_t lookahead; size_t adv = peekUchar(&lookahead, st->rest);
    if (lookahead == escapeLeader) {
      indented = true;
      lexer_advance(st, adv, 1);
      trailingSpace = false;
      err.loc.start = st->loc;
      while (true) {
        char32_t c; size_t adv = peekUchar(&c, st->rest);
        if (isSpaceChar(c)) {
          lexer_advance(st, adv, 1);
          trailingSpace = true;
        }
        else { break; }
      }
    }
    if (trailingSpace) {
      err.loc.end = st->loc;
      dllist_insertAfter_eexpr_error(&st->errStream, NULL, &err);
    }
  }
  { // consume a newline, or else it's a fatal error
    if (takeNewline(st)) {
      lexer_delTok(st);
    }
    else {
      st->fatal.type = EEXPRERR_HEREDOC_BAD_OPEN;
      st->fatal.loc.start = tok.loc.start;
      st->fatal.loc.end = st->loc;
    }
  }
  char32_t indentChar;
  size_t indentNChars = 0;
  { // accumulate indentation
    if (indented) {
      // determine indentation character
      struct eexpr_locPoint indentPosStart = st->loc;
      char32_t c; size_t adv = peekUchar(&c, st->rest);
      if (isSpaceChar(c)) {
        lexer_advance(st, adv, 1);
        indentChar = c;
        indentNChars += 1;
      }
      else {
        goto badIndentDef;
      }
      // count indentation depth
      while (true) {
        char32_t c; size_t adv = peekUchar(&c, st->rest);
        if (c == indentChar) {
          lexer_advance(st, adv, 1);
          indentNChars += 1;
        }
        else if (c == escapeLeader) {
          lexer_advance(st, adv, 1);
          indentNChars += 1;
          if (c == tabChar) {
            // tab-based indentation needs an alignment tab after the closing backslash
            char32_t c; size_t adv = peekUchar(&c, st->rest);
            if (c == tabChar) {
              lexer_advance(st, adv, 1);
            }
            else {
              goto badIndentDef;
            }
          }
          break;
        }
        else badIndentDef: {
          tok.type = EEXPR_TOK_STRING_ERROR;
          tok.loc.end = st->loc;
          lexer_addTok(st, &tok);
          st->fatal.type = EEXPRERR_HEREDOC_BAD_INDENT_DEFINITION;
          st->fatal.loc = tok.loc;
          return true;
        }
      }
      // make sure we aren't already mixing indentation
      if (st->indent.chr == UCHAR_NULL) {
        // this is the first time we've seen indentation
        st->indent.chr = indentChar;
        st->indent.established.start = indentPosStart;
        st->indent.established.end = st->loc;
      }
      else if (indentChar != st->indent.chr && !st->indent.knownMixed) {
        eexpr_error err = {.loc = {.start = indentPosStart, .end = st->loc}, .type = EEXPRERR_MIXED_INDENTATION};
        err.as.mixedIndentation.chr = st->indent.chr;
        err.as.mixedIndentation.loc = st->indent.established;
        st->indent.knownMixed = true;
        dllist_insertAfter_eexpr_error(&st->errStream, NULL, &err);
      }
    }
    else {
      indentChar = UCHAR_NULL;
    }
  }
  // accumulate lines until end marker
  strBuilder textBuf = strBuilder_new(256);
  while (true) {
    { // consume line
      str tmp = {.len = 0, .bytes = st->rest.bytes};
      while (true) {
        char32_t c; size_t adv = peekUchar(&c, st->rest);
        if ( adv == 0
          || isNewlineChar(c)
           ) {
          strBuilder_append(&textBuf, tmp);
          break;
        }
        else if (c == UCHAR_NULL) {
          strBuilder_append(&textBuf, tmp);
          tryBadBytes(st, false);
          tmp.len = 0; tmp.bytes = st->rest.bytes;
        }
        else {
          lexer_advance(st, adv, 1);
          tmp.len += adv;
        }
      }
    }
    str nlText = {.len = 0, .bytes = st->rest.bytes};
    { // consume newline
      if (takeNewline(st)) {
        lexer_delTok(st);
        nlText.len = st->rest.bytes - nlText.bytes;
      }
      else {
        free(ender.bytes);
        tok.type = EEXPR_TOK_STRING_ERROR;
        tok.loc.end = st->loc;
        lexer_addTok(st, &tok);
        st->fatal.type = EEXPRERR_UNCLOSED_MULTILINE_STRING;
        st->fatal.loc = tok.loc;
        return true;
      }
    }
    { // consume indentation
      eexpr_error err = {.loc = {.start = st->loc}, .type = EEXPRERR_HEREDOC_BAD_INDENTATION};
      for (size_t i = 0; i < indentNChars; ++i) {
        char32_t c; size_t adv = peekUchar(&c, st->rest);
        if (c == indentChar) {
          lexer_advance(st, adv, 1);
        }
        else if (isNewlineChar(c)) {
          if (i != 0) {
            err.type = EEXPRERR_TRAILING_SPACE;
            err.loc.end = st->loc;
            dllist_insertAfter_eexpr_error(&st->errStream, NULL, &err);
          }
          break;
        }
        else {
          err.loc.end = st->loc;
          dllist_insertAfter_eexpr_error(&st->errStream, NULL, &err);
          break;
        }
      }
    }
    { // detect end-of-heredoc
      if (isPrefixOf(st->rest, ender)) {
        lexer_advance(st, ender.len, enderNChars);
        break;
      }
      else {
        strBuilder_append(&textBuf, nlText);
      }
    }
  }
  free(ender.bytes);
  tok.loc.end = st->loc;
  tok.as.string.text.len = textBuf.len;
  tok.as.string.text.bytes = textBuf.bytes;
  assert(tok.as.string.text.bytes != NULL);
  lexer_addTok(st, &tok);
  return true;
}

/*
Wraps are parens, brackets, and braces.
  `[()\[\]{}]`
*/
static
bool takeWrap(engine* st) {
  char32_t lookahead;
  size_t adv = peekUchar(&lookahead, st->rest);
  eexpr_wrapType type = isWrapChar(lookahead);
  if (type == WRAP_NULL) { return false; }
  eexpr_token tok = {.loc = {.start = st->loc}, .type = EEXPR_TOK_WRAP};
  {
    tok.as.wrap.type = type;
    tok.as.wrap.isOpen = isOpenWrap(lookahead);
  }
  lexer_advance(st, adv, 1);
  tok.loc.end = st->loc;
  lexer_addTok(st, &tok);
  return true;
}

/*
Splitters are colon, dot, ellipsis, semicolon, and comma.
  `\.\.[:.;,]`
*/
static
bool takeSplitter(engine* st) {
  splitter info;
  {
    char32_t lookahead[2];
    peekUchars(lookahead, 2, st->rest);
    info = decodeSplitter(lookahead);
    if (info.type == SPLITTER_NONE) { return false; }
  }
  eexpr_token tok = {.loc = {.start = st->loc}};
  switch (info.type) {
    case SPLITTER_COLON: tok.type = EEXPR_TOK_UNKNOWN_COLON; break;
    case SPLITTER_ELLIPSIS: tok.type = EEXPR_TOK_ELLIPSIS; break;
    case SPLITTER_DOT: tok.type = EEXPR_TOK_UNKNOWN_DOT; break;
    case SPLITTER_SEMICOLON: tok.type = EEXPR_TOK_SEMICOLON; break;
    case SPLITTER_COMMA: tok.type = EEXPR_TOK_COMMA; break;
    default: assert(false);
  }
  lexer_advance(st, info.bytes, info.chars);
  tok.loc.end = st->loc;
  lexer_addTok(st, &tok);
  return true;
}

// always consumes a character (or byte if the remaining input is not valid utf8)
static
bool takeUnexpected(engine* st) {
  char32_t c;
  size_t adv = peekUchar(&c, st->rest);
  if (c == UCHAR_NULL && adv != 0) {
    tryBadBytes(st, false);
    return true;
  }
  else {
    eexpr_error err = {.loc = {.start = st->loc}, .type = EEXPRERR_BAD_CHAR};
    err.as.badChar = c;
    lexer_advance(st, adv, 1);
    err.loc.end = st->loc;
    dllist_insertAfter_eexpr_error(&st->errStream, NULL, &err);
    return true;
  }
}

//////////////////////////////////// Main Lexer Functions ////////////////////////////////////

void engine_rawLex(engine* st) {
  while (st->fatal.type == EEXPRERR_NOERROR) {
    if (takeWhitespace(st)) { continue; }
    if (takeNewline(st)) { continue; }
    if (takeComment(st)) { continue; }
    if (takeSymbol(st)) { continue; }
    if (takeNumber(st)) { continue; }
    if (takeHeredoc(st)) { continue; }
    if (takeString(st)) { continue; }
    // if (takeCodepoint(st)) { continue; }
    if (takeSqlString(st)) { continue; }
    if (takeSplitter(st)) { continue; }
    if (takeWrap(st)) { continue; }
    if (takeLineContinue(st)) { continue; }
    if (takeEof(st)) { break; }
    if (takeUnexpected(st)) { continue; }
    assert(false);
  }
}
