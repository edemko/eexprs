#include "parser/util.h"

#include <assert.h>
#include <stdlib.h>

#include "shim/common.h"


void eexprStream_del(eexprStream* strm) {
  for (size_t i = 0; i < strm->len; ++i) {
    eexpr_deinit(strm->buf[i]);
  }
  strm->len = 0;
}

void deleteFront(parser* st) {
  tokenStream* head = st->tokStream;
  if (head->next != NULL) {
    head->next->prev = NULL;
  }
  st->tokStream = head->next;
  if (st->tokStream == NULL) {
    st->tokStream_end = NULL;
  }
  free(head); // dont' delete the token, because it's assumed that ownership has been transferred
}

token* parser_peek(parser* st) {
  tokenStream* strm = st->tokStream;
  while (strm != NULL) {
    if (strm->here.transparent) {
      deleteFront(st);
      strm = st->tokStream;
    }
    else {
      return &strm->here;
    }
  }
  return NULL;
}

void parser_pop(parser* st) {
  tokenStream* strm = st->tokStream;
  assert(strm != NULL);
  while (strm->here.transparent) {
    deleteFront(st);
    strm = st->tokStream;
    assert(strm != NULL);
  }
  assert(!strm->here.transparent);
  deleteFront(st);
}

void parser_addEexpr(parser* st, const eexpr* expr) {
  assert(st->eexprStream != NULL);
  eexprStream* eexprs = st->eexprStream;
  if (eexprs->len >= eexprs->cap) {
    assert(eexprs->cap > 0);
    eexprs->cap *= 2;
    st->eexprStream = realloc(st->eexprStream, sizeof(eexprStream) + eexprs->cap * sizeof(eexpr));
    checkOom(st->eexprStream);
  }
  eexprs->buf[eexprs->len++] = *expr;
}
