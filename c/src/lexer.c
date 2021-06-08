#include <assert.h>
#include <string.h>

#include "common.h"
#include "lexer.h"
#include "parameters.h"


lexer lexer_newFromFile(const char* filename) {
  lexer out;
  out.rest = readFile(filename);
  out.loc.line = 0;
  out.loc.col = 0;
  out.startOfInput = out.rest.bytes;
  {
    out.lineIndex.len = 1;
    out.lineIndex.cap = 256;
    out.lineIndex.offsets = malloc(sizeof(size_t) * 256);
    checkOom(out.lineIndex.offsets);
    out.lineIndex.offsets[0] = 0;
  }
  return out;
}


//////////////////////////////////// Helper Consumers ////////////////////////////////////

// decode a hex-encoded unicode codepoint into `out`
// if decoding fails, return false and do not modify `out`
bool decodeUnihex(size_t nChars, uchar* out, uchar* digits) {
  uchar accum = 0;
  for (int i = 0; i < nChars; ++i) {
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
// returns a single uchar, or UCHAR_NULL if no valid escape sequence is found
// input is consumed and errors are emitted (escpe no error is emitted if no valid escape is found; allows chaining with takeStrEscape)
// call only after detecting an escapeLeader
uchar takeCharEscape(lexer* st) {
  uchar c;
  size_t adv;
  adv = peekUchar(&c, st->rest);
  // standard escapes
  for (size_t i = 0; commonEscapes[i].source != UCHAR_NULL; ++i) {
    if (c == commonEscapes[i].source) {
      lexer_advance(st, adv, 1);
      return commonEscapes[i].decode;
    }
  }
  uchar digits[6] = {UCHAR_NULL, UCHAR_NULL, UCHAR_NULL, UCHAR_NULL, UCHAR_NULL, UCHAR_NULL};
  lexError decodeError = {.loc = {.start = st->loc}, .type = LEXERR_BAD_ESCAPE_CODE};
  if (c == twoHexEscapeLeader) {
    lexer_advance(st, adv, 1);
    adv = peekUchars(&digits[4], 2, st->rest);
    lexer_advance(st, adv, 2);
    if (!decodeUnihex(2, &c, &digits[4])) {
      decodeError.loc.end = st->loc;
      for (int i = 0; i < 6; ++i) { decodeError.as.badEscapeCode[i] = digits[i]; };
      lexer_addErr(st, &decodeError);
    }
    return c;
  }
  else if (c == fourHexEscapeLeader) {
    lexer_advance(st, adv, 1);
    adv = peekUchars(&digits[2], 4, st->rest);
    lexer_advance(st, adv, 4);
    if (!decodeUnihex(4, &c, &digits[2])) {
      decodeError.loc.end = st->loc;
      for (int i = 0; i < 6; ++i) { decodeError.as.badEscapeCode[i] = digits[i]; };
      lexer_addErr(st, &decodeError);
    }
    return c;
  }
  else if (c == sixHexEscapeLeader) {
    lexer_advance(st, adv, 1);
    adv = peekUchars(digits, 6, st->rest);
    lexer_advance(st, adv, 6);
    if (!decodeUnihex(6, &c, digits)) {
      decodeError.loc.end = st->loc;
      for (int i = 0; i < 6; ++i) { decodeError.as.badEscapeCode[i] = digits[i]; };
      lexer_addErr(st, &decodeError);
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
  `\\[:linebreak:][:whitespace::comment::linebreak:]*\\`

Comments/whitespace are not allowed before the first linebreak to keep lookahead constant-space.
*/
// As `takeCharEscape`, but returns true iff characters were consumed.
bool takeNewline(lexer* st);
bool takeWhitespace(lexer* st);
bool takeComment(lexer* st);
bool takeNullEscape(lexer* st) {
  uchar c;
  size_t adv;
  adv = peekUchar(&c, st->rest);
  if (isNewlineChar(c)) {
    assert(takeNewline(st));
    lexer_delTok(st);
    for (bool more; more; ) { // continue taking newlines, whitespaces, and comments
      more = false;
      if (takeWhitespace(st)) { lexer_delTok(st); more = true; }
      if (takeComment(st))    { lexer_delTok(st); more = true; }
      if (takeNewline(st))    { lexer_delTok(st); more = true; }
    }
    adv = peekUchar(&c, st->rest);
    if (c == escapeLeader) {
      lexer_advance(st, adv, 1);
    }
    else {
      lexError err = {.loc = {.start = st->loc, .end = st->loc}, .type = LEXERR_MISSING_LINE_PICKUP};
      lexer_addErr(st, &err);
    }
    return true;
  }
  else if (c == nullEscape) {
    lexer_advance(st, adv, 1);
    return true;
  }
  return false;
}


//////////////////////////////////// Individual Token Consumers ////////////////////////////////////

/*
(Inline) whitespace is simply one or more whitespace characters of any type.
  `[:whitespaceChar:]+`
However, if the whitespace is not simply a repetition of the same character, that it a "mixed whitespace" error.
*/
bool takeWhitespace(lexer* st) {
  {
    uchar lookahead;
    peekUchar(&lookahead, st->rest);
    if (!isSpaceChar(lookahead)) { return false; }
  }
  token tok = {.loc = {.start = st->loc}, .type = TOK_UNKNOWN_SPACE};
  uchar c0; peekUchar(&c0, st->rest);
  bool mixed = false;
  bool charsWereConsumed = false;
  while (true) {
    uchar c;
    size_t adv = peekUchar(&c, st->rest);
    if (isSpaceChar(c)) {
      lexer_advance(st, adv, 1);
      mixed |= c != c0;
      charsWereConsumed = true;
    }
    else {
      break;
    }
  }
  assert(charsWereConsumed);
  tok.loc.end = st->loc;
  tok.as.space.chr = mixed ? UCHAR_SENTINEL : c0;
  lexer_addTok(st, &tok);
  if (mixed) {
    lexError err =
      { .loc = tok.loc
      , .type = LEXERR_MIXED_SPACE
      };
    lexer_addErr(st, &err);
  }
  return true;
}

/*
Newlines are one of the newline sequences (i.e. alternation of literals).
See `parameters.c` for valid newline sequences.
However, if the newlines of a file are not all the same sequence, that it a "mixed newlines" error.
*/
bool takeNewline(lexer* st) {
  newlineType type;
  {
    uchar lookahead[2];
    peekUchars(lookahead, 2, st->rest);
    type = decodeNewline(lookahead) != 0;
    if (type == NEWLINE_NONE) { return false; }
  }
  token tok = {.loc = {.start = st->loc}, .type = TOK_UNKNOWN_NEWLINE};
  lexer_incLine(st, newlineSize(type));
  tok.loc.end = st->loc;
  lexer_addTok(st, &tok);
  if (type != st->discoveredNewline) {
    if (st->discoveredNewline == NEWLINE_NONE) {
      st->discoveredNewline = type;
    }
    else {
      lexError err =
      { .loc = tok.loc
      , .type = LEXERR_MIXED_NEWLINES
      };
      lexer_addErr(st, &err);
    }
  }
  return true;
}

/*
  The end of the file also counts as a newline.
*/
bool takeEof(lexer* st) {
  uchar c;
  peekUchar(&c, st->rest);
  return c == UCHAR_NULL;
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
bool takeComment(lexer* st) {
  {
    uchar lookahead;
    peekUchar(&lookahead, st->rest);
    if (lookahead != commentChar) { return false; }
  }
  token tok = {.loc = {.start = st->loc}, .type = TOK_COMMENT};
  struct untilEol skip = untilEol(st->rest);
  lexer_advance(st, skip.bytes, skip.uchars);
  tok.loc.end = st->loc;
  lexer_addTok(st, &tok);
  {
    uchar lookahead;
    peekUchar(&lookahead, st->rest);
    if (lookahead < 0 && lookahead != UCHAR_NULL) {
      lexError* err = malloc(sizeof(lexError));
      checkOom(err);
      err->type = LEXERR_BAD_BYTES;
      err->loc.start = st->loc;
      lexer_advance(st, 1, 1);
      err->loc.end = st->loc;
      // b/c at this point I don't know what to beleive about where the end-of-line is supposed to be
      st->fatal = err;
    }
  }
  return true;
}

/*
Symbols are simply one or more symbol characters.
  `[:symbolChar:]+`
*/
bool takeSymbol(lexer* st) {
  {
    uchar lookahead[2];
    peekUchars(lookahead, 2, st->rest);
    if (!isSymbolStart(lookahead)) { return false; }
  }
  str text = { .len = 0, .bytes = st->rest.bytes };
  token tok = {.loc = {.start = st->loc}, .type = TOK_SYMBOL};
  while (true) {
    uchar c;
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

void checkDigitSepContext(const radixParams* radix, filelocPoint start, bool alwaysError, lexer* st) {
  uchar lookahead;
  peekUchar(&lookahead, st->rest);
  if ( alwaysError
    || (!isDigit(radix, lookahead) && lookahead != digitSep)
     ) {
    lexError err = {.loc = {.start = start, .end = st->loc}, .type = LEXERR_BAD_DIGIT_SEPARATOR};
    lexer_addErr(st, &err);
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
bool takeNumber(lexer* st) {
  token tok = {.loc = {.start = st->loc}, .type = TOK_NUMBER};
  ////// gather sign (or early exit) //////
  bool neg;
  {
    uchar lookahead[2];
    size_t adv = peekUchars(lookahead, 1, st->rest);
    if (isDigit(defaultRadix, lookahead[0])) {
      neg = false;
    }
    else if (isSign(lookahead[0])) {
      neg = lookahead[0] == '-';
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
    uchar lookahead[2];
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
      uchar c;
      size_t adv = peekUchar(&c, st->rest);
      if (isDigit(radix, c)) {
        lexer_advance(st, adv, 1);
        bigint_scale(&mantissa, radix->radix);
        bigint_inc(&mantissa, decodeDigit(radix, c));
        integerDigits += 1;
      }
      else if (c == digitSep) {
        filelocPoint loc0 = st->loc;
        lexer_advance(st, adv, 1);
        checkDigitSepContext(radix, loc0, integerDigits == 0, st);
      }
      else { break; }
    }
  }
  ////// gather fractional part //////
  uint32_t fractionalDigits = 0;
  { // decimal point
    uchar lookahead;
    size_t adv = peekUchar(&lookahead, st->rest);
    if (lookahead == digitPoint) {
      lexer_advance(st, adv, 1);
      peekUchar(&lookahead, st->rest);
      if (isDigit(radix, lookahead)) {
        while (true) {
          uchar c;
          size_t adv = peekUchar(&c, st->rest);
          if (isDigit(radix, c)) {
            lexer_advance(st, adv, 1);
            bigint_scale(&mantissa, radix->radix);
            bigint_inc(&mantissa, decodeDigit(radix, c));
            fractionalDigits += 1;
          }
          else if (c == digitSep) {
            filelocPoint loc0 = st->loc;
            lexer_advance(st, adv, 1);
            checkDigitSepContext(radix, loc0, fractionalDigits == 0, st);
          }
          else { break; }
        }
      }
      else {
        lexError err = {.loc = {.start = tok.loc.start, .end = st->loc}, .type = LEXERR_MISSING_FRACTIONAL_PART};
        lexer_addErr(st, &err);
        return true;
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
      uchar lookahead;
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
        uchar lookahead;
        size_t adv = peekUchar(&lookahead, st->rest);
        if (isSign(lookahead)) {
          if (fractionalDigits) {
            expNeg = lookahead == '-';
            lexer_advance(st, adv, 1);
          }
          else {
            expNeg = false;
            lexError err = {.loc = {.start = st->loc}, .type = LEXERROR_BAD_EXPONENT_SIGN};
            lexer_advance(st, adv, 1);
            err.loc.end = st->loc;
            lexer_addErr(st, &err);
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
        uchar lookahead[2];
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
          uchar c;
          size_t adv = peekUchar(&c, st->rest);
          if (isDigit(expRadix, c)) {
            expDigits += 1;
            lexer_advance(st, adv, 1);
            bigint_scale(&exponent, expRadix->radix);
            bigint_inc(&exponent, decodeDigit(expRadix, c));
          }
          else if (c == digitSep) {
            filelocPoint loc0 = st->loc;
            lexer_advance(st, adv, 1);
            checkDigitSepContext(expRadix, loc0, expDigits == 0, st);
          }
          else { break; }
        }
        if (expDigits == 0) {
          lexError err = {.loc = {.start = tok.loc.start, .end = st->loc}, .type = LEXERROR_MISSING_EXPONENT};
          lexer_addErr(st, &err);
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
A codepoint literal is any (reasonable) character between single-ticks.
Escape sequences are also permitted, as long as they encode exactly one codepoint.
  `'[:stringChar::charEscape:]'`
FIXME: the question is, do I even want codepoint literals if I could just `fromString` them?
*/
bool takeCodepoint(lexer* st) {
  token tok = {.loc = {.start = st->loc}, .type = TOK_CODEPOINT, .as = {.codepoint = {.chr = UCHAR_NULL}}};
  {
    uchar lookahead;
    size_t adv = peekUchar(&lookahead, st->rest);
    if (!isCodepointDelim(lookahead)) { return false; }
    lexer_advance(st, adv, 1);
  }
  { // obtain a single codepoint
    { // standard character
      uchar c;
      size_t adv = peekUchar(&c, st->rest);
      if (isStringChar(c)) {
        tok.as.codepoint.chr = c;
        lexer_advance(st, adv, 1);
        goto codepointBuilt;
      }
    }
    { // single-codepoint escape sequence
      uchar c;
      size_t adv = peekUchar(&c, st->rest);
      if (c == escapeLeader) {
        lexer_advance(st, adv, 1);
        tok.as.codepoint.chr = takeCharEscape(st);
        if (tok.as.codepoint.chr == UCHAR_NULL) {
          lexError escapeCharErr = {.loc = {.start = st->loc}, .type = LEXERR_BAD_ESCAPE_CHAR};
          adv = peekUchar(&escapeCharErr.as.badEscapeChar, st->rest);
          if (isCodepointDelim(escapeCharErr.as.badEscapeChar)) { // don't consume the next char if it's a tick
            escapeCharErr.as.badEscapeChar = UCHAR_NULL;
          }
          else if (escapeCharErr.as.badEscapeChar != UCHAR_NULL) { // don't try to consume eof
            lexer_advance(st, adv, 1);
          }
          lexer_addErr(st, &escapeCharErr);
        }
        goto codepointBuilt;
      }
    }
    { // no valid codepoint found
      tok.as.codepoint.chr = UCHAR_NULL;
      lexError codepointErr = {.loc = {.start = st->loc}, .type = LEXERR_BAD_CODEPOINT};
      size_t adv = peekUchar(&codepointErr.as.badCodepoint, st->rest);
      if (isCodepointDelim(codepointErr.as.badCodepoint)) { // don't consume the next char if it's a tick
        codepointErr.as.badCodepoint = UCHAR_NULL;
      }
      else if (codepointErr.as.badCodepoint != UCHAR_NULL) { // don't try to consume eof
        lexer_advance(st, adv, 1);
      }
      codepointErr.loc.end = st->loc;
      lexer_addErr(st, &codepointErr);
      goto codepointBuilt;
    }
  }; codepointBuilt:
  { // closing tick, or error recovery
    {
      uchar lookahead;
      size_t adv = peekUchar(&lookahead, st->rest);
      if (isCodepointDelim(lookahead)) {
        lexer_advance(st, adv, 1);
        goto tokenClosed;
      }
    }
    {
      lexError noCloseErr = {.loc = {.start = st->loc}, .type = LEXERR_UNCLOSED_CODEPOINT};
      while (true) {
        uchar c;
        size_t adv = peekUchar(&c, st->rest);
        lexer_advance(st, adv, c == UCHAR_NULL ? 0 : 1);
        if ( isCodepointDelim(c)
          || isNewlineChar(c)
          || c == UCHAR_NULL
           ) {
          break;
        }
      }
      noCloseErr.loc.end = st->loc;
      lexer_addErr(st, &noCloseErr);
      goto tokenClosed;
    }
  } tokenClosed:
  if (tok.as.codepoint.chr > 0x10FFFF) {
    lexError overflowErr = {.loc = tok.loc, .type = LEXERR_UNICODE_OVERFLOW};
    overflowErr.as.unicodeOverflow = tok.as.codepoint.chr;
    tok.as.codepoint.chr = UCHAR_NULL;
    lexer_addErr(st, &overflowErr);
  }
  if (tok.as.codepoint.chr >= 0) {
    tok.loc.end = st->loc;
    lexer_addTok(st, &tok);
  }
  return true;
}

bool takeString(lexer* st) {
  token tok = {.loc = {.start = st->loc}, .type = TOK_STRING};
  uchar open; {
    size_t adv = peekUchar(&open, st->rest);
    if (!isStringDelim(open)) { return false; }
    lexer_advance(st, adv, 1);
  }
  size_t cap = 128;
  size_t len = 0;
  uint8_t* buf = malloc(cap);
  checkOom(buf);
  for (bool more = true; more; ) {
    more = false;
    { // standard characters
      size_t advLen = 0;
      uint8_t* start = st->rest.bytes;
      while (true) {
        uchar c;
        size_t adv = peekUchar(&c, st->rest);
        if (!isStringChar(c)) { break; }
        lexer_advance(st, adv, 1);
        advLen += adv;
      }
      if (advLen != 0) {
        more = true;
        if (cap - len < advLen) {
          while (cap - len < advLen) { cap *= 2; }
          buf = realloc(buf, cap);
          checkOom(buf);
        }
        memcpy(&buf[len], start, advLen);
        len += advLen;
      }
    }
    { // escape sequences
      uchar c;
      size_t adv = peekUchar(&c, st->rest);
      if (c == escapeLeader) {
        lexer_advance(st, adv, 1);
        more = true;
        uchar decoded = takeCharEscape(st);
        if (decoded != UCHAR_NULL) { // found a single-character escape
          utf8Char encoded = encodeUchar(decoded);
          if (cap - len < encoded.nbytes) {
            cap *= 2;
            buf = realloc(buf, cap);
            checkOom(buf);
          }
          memcpy(&buf[len], encoded.codeunits, encoded.nbytes);
          len += encoded.nbytes;
        } else if (takeNullEscape(st)) { // found a null escape
          // do nothing
        }
        else { // no valid escape sequence found
          lexError escapeCharErr = {.loc = {.start = st->loc}, .type = LEXERR_BAD_ESCAPE_CHAR};
          adv = peekUchar(&escapeCharErr.as.badEscapeChar, st->rest);
          if (isStringDelim(escapeCharErr.as.badEscapeChar)) { // don't consume the next char if it's a string delimiter
            escapeCharErr.as.badEscapeChar = UCHAR_NULL;
          }
          else if (escapeCharErr.as.badEscapeChar != UCHAR_NULL) { // don't try to consume eof
            lexer_advance(st, adv, 1);
          }
          lexer_addErr(st, &escapeCharErr);
        }
      }
    }
    {
      uchar c;
      size_t adv = peekUchar(&c, st->rest);
      // stop at close delimiter or end of line/file
      if ( c == UCHAR_NULL
        || isStringDelim(c)
        || isNewlineChar(c)
        ) { break; }
      else if (!more) { // characters that did not match above are invalid and error recovery should skip them
        lexError err = {.loc = {.start = st->loc}, .type = LEXERR_BAD_STRING_CHAR, .as.badStringChar = c};
        lexer_advance(st, adv, 1);
        err.loc.end = st->loc;
        lexer_addErr(st, &err);
      }
    }
  }
  uchar close; {
    size_t adv = peekUchar(&close, st->rest);
    if (isStringDelim(close)) {
      lexer_advance(st, adv, 1);
    }
    else {
      lexError err = {.loc = {.start = st->loc, .end = st->loc}, .type = LEXERR_UNCLOSED_STRING};
      lexer_addErr(st, &err);
    }
  }
  tok.loc.end = st->loc;
  tok.as.string.text.len = len;
  tok.as.string.text.bytes = realloc(buf, len);
  tok.as.string.splice = spliceType(open, close);
  lexer_addTok(st, &tok);
  return true;
}

/*
Wrappers are parens, brackets, and braces.
  `[()\[\]{}]`
*/
bool takeWrapper(lexer* st) {
  uchar lookahead;
  size_t adv = peekUchar(&lookahead, st->rest);
  if (!isWrapChar(lookahead)) { return false; }
  token tok = {.loc = {.start = st->loc}, .type = TOK_WRAPPER};
  {
    tok.as.wrapper.chr = openWrapper(lookahead);
    tok.as.wrapper.isOpen = tok.as.wrapper.chr == lookahead;
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
bool takeSplitter(lexer* st) {
  splitter info;
  {
    uchar lookahead[2];
    peekUchars(lookahead, 2, st->rest);
    info = decodeSplitter(lookahead);
    if (info.type == SPLITTER_NONE) { return false; }
  }
  token tok = {.loc = {.start = st->loc}};
  switch (info.type) {
    case SPLITTER_COLON: tok.type = TOK_UNKNOWN_COLON; break;
    case SPLITTER_ELLIPSIS: tok.type = TOK_ELLIPSIS; break;
    case SPLITTER_DOT: tok.type = TOK_UNKNOWN_DOT; break;
    case SPLITTER_SEMICOLON: tok.type = TOK_SEMICOLON; break;
    case SPLITTER_COMMA: tok.type = TOK_COMMA; break;
    default: assert(false);
  }
  lexer_advance(st, info.bytes, info.uchars);
  tok.loc.end = st->loc;
  lexer_addTok(st, &tok);
  return true;
}

// always consumes a character (or byte if the remaining input is not valid utf8)
bool takeUnexpected(lexer* st) {
  uchar c;
  size_t adv = peekUchar(&c, st->rest);
  lexError err = {.loc = {.start = st->loc}};
  if (c < 0 || c > 0x10FFFF) {
    err.type = LEXERR_BAD_BYTES;
    lexer_advance(st, adv, 0);
  }
  else {
    err.type = LEXERR_BAD_CHAR;
    err.as.badChar = c;
    lexer_advance(st, adv, 1);
  }
  err.loc.end = st->loc;
  lexer_addErr(st, &err);
  return true;
}

//////////////////////////////////// Main Lexer Functions ////////////////////////////////////

void lexer_raw(lexer* st) {
  while (st->fatal == NULL) {
    if (takeWhitespace(st)) { continue; }
    if (takeNewline(st)) { continue; }
    if (takeComment(st)) { continue; }
    if (takeSymbol(st)) { continue; }
    if (takeNumber(st)) { continue; }
    if (takeString(st)) { continue; }
    if (takeCodepoint(st)) { continue; }
    if (takeWrapper(st)) { continue; }
    if (takeSplitter(st)) { continue; }
    if (takeEof(st)) { break; }
    if (takeUnexpected(st)) { continue; }
    assert(false);
  }
}

void lexer_advance(lexer* st, size_t bytes, size_t cols) {
  st->rest.len -= bytes;
  st->rest.bytes += bytes;
  st->loc.col += cols;
}
void lexer_incLine(lexer* st, size_t bytes) {
  st->rest.len -= bytes;
  st->rest.bytes += bytes;
  st->loc.col = 0;
  st->loc.line += 1;
  {
    if (st->lineIndex.len == st->lineIndex.cap) {
      size_t* newBuf = realloc(st->lineIndex.offsets, 2 * st->lineIndex.cap);
      checkOom(newBuf);
      st->lineIndex.offsets = newBuf;
    }
    st->lineIndex.offsets[st->lineIndex.len++] = st->rest.bytes - st->startOfInput;
  }
}

void lexer_addTok(lexer* st, const token* t) {
  tokenStream* new = malloc(sizeof(tokenStream));
  checkOom(new);
  new->here = *t;
  new->next = NULL;
  if (st->outStream != NULL) {
    new->prev = st->outStream_end;
    st->outStream_end->next = new;
    st->outStream_end = new;
  }
  else {
    new->prev = NULL;
    st->outStream = new;
    st->outStream_end = new;
  }
}

void lexer_delTok(lexer* st) {
  tokenStream* last = st->outStream_end;
  assert(last != NULL);
  st->outStream_end = last->prev;
  st->outStream_end->next = NULL;
  free(last);
}

void lexer_addErr(lexer* st, const lexError* err) {
  lexErrStream* new = malloc(sizeof(lexErrStream));
  checkOom(new);
  new->here = *err;
  new->next = NULL;
  if (st->errStream != NULL) {
    new->prev = st->errStream_end;
    st->errStream_end->next = new;
    st->errStream_end = new;
  }
  else {
    new->prev = NULL;
    st->errStream = new;
    st->errStream_end = new;
  }
}
