#include <assert.h>

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
  if (c != UCHAR_NULL) { return false; }
  token tok = {.loc = {.start = st->loc, .end = st->loc}, .type = TOK_UNKNOWN_NEWLINE};
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
    err.as.badChar.chr = c;
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
    // TODO numbers, strings, chars
    // do chars before strings so I have escapes already figured out
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
