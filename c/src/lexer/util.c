#include <assert.h>
#include <string.h>

#include "lexer/util.h"
#include "shim/common.h"


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
    if (st->lineIndex.len >= st->lineIndex.cap) {
      assert(st->lineIndex.cap != 0);
      st->lineIndex.cap *= 2;
      size_t* newBuf = realloc(st->lineIndex.offsets, st->lineIndex.cap);
      checkOom(newBuf);
      st->lineIndex.offsets = newBuf;
    }
    st->lineIndex.offsets[st->lineIndex.len++] = st->rest.bytes - st->allInput.bytes;
  }
}

void lexer_addTok(lexer* st, const token* t) {
  tokenStream* new = malloc(sizeof(tokenStream));
  checkOom(new);
  new->here = *t;
  new->here.transparent = false;
  new->prev = st->tokStream_end;
  if (st->tokStream == NULL) { st->tokStream = new; } else { st->tokStream_end->next = new; }
  st->tokStream_end = new;
  new->next = NULL;
}

void lexer_insertBefore(lexer* st, const token* t, tokenStream* point) {
  tokenStream* new = malloc(sizeof(tokenStream));
  checkOom(new);
  new->here = *t;
  new->here.transparent = false;
  new->next = point;
  new->prev = point->prev;
  if (point->prev != NULL) { point->prev->next = new; }
  else { st->tokStream = new; }
  point->prev = new;

}

void lexer_delTok(lexer* st) {
  tokenStream* last = st->tokStream_end;
  assert(last != NULL);
  st->tokStream_end = last->prev;
  st->tokStream_end->next = NULL;
  free(last);
}

void lexer_addErr(lexer* st, const lexError* err) {
  lexErrStream* new = malloc(sizeof(lexErrStream));
  checkOom(new);
  new->here = *err;
  new->prev = st->errStream_end;
  if (st->errStream == NULL) { st->errStream = new; } else { st->errStream_end->next = new; }
  st->errStream_end = new;
  new->next = NULL;
}

void lexer_fatalErr(lexer* st, const lexError* err) {
  st->fatal = malloc(sizeof(lexError));
  checkOom(st->fatal);
  *st->fatal = *err;
}

void lexer_errToWarn(lexer* st, lexErrStream* err) {
  if (err->prev == NULL) { st->errStream = err->next; } else { err->prev->next = err->next; }
  if (err->next == NULL) { st->errStream_end = err->prev; } else { err->next->prev = err->prev; }
  err->prev = st->warnStream_end;
  if (st->warnStream == NULL) { st->warnStream = err; } else { st->warnStream_end->next = err; }
  st->warnStream_end = err;
  err->next = NULL;
}


void tokenStream_del(tokenStream* strm) {
  while (strm != NULL) {
    switch (strm->here.type) {
      case TOK_STRING: {
        if (strm->here.as.string.text.bytes != NULL) { free(strm->here.as.string.text.bytes); }
      }; break;
      case TOK_SYMBOL: {
        if (strm->here.as.symbol.text.bytes != NULL) { free(strm->here.as.symbol.text.bytes); }
      }; break;
      case TOK_NUMBER: {
        if (strm->here.as.number.mantissa.buf != NULL) { free(strm->here.as.number.mantissa.buf); }
        if (strm->here.as.number.exponent.buf != NULL) { free(strm->here.as.number.exponent.buf); }
      }; break;
      default: /* do nothing */ break;
    }
    tokenStream* next = strm->next;
    free(strm);
    strm = next;
  }
}
void lexErrStream_del(lexErrStream* strm) {
  while (strm != NULL) {
    lexErrStream* next = strm->next;
    free(strm);
    strm = next;
  }
}
