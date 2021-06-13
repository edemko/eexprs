#include "parser/util.h"

#include <assert.h>
#include <stdlib.h>

#include "shim/common.h"


token* parser_peek(parser* st) {
  dllistNode(token)* node = st->tokStream.start;
  while (node != NULL) {
    if (node->here.transparent) {
      token_deinit(&node->here);
      dllist_popStart(token)(&st->tokStream, NULL);
      node = st->tokStream.start;
    }
    else {
      return &node->here;
    }
  }
  return NULL;
}

void parser_pop(parser* st) {
  dllistNode(token)* node = st->tokStream.start;
  while (node != NULL) {
    if (node->here.transparent) {
      token_deinit(&node->here);
      dllist_popStart(token)(&st->tokStream, NULL);
      node = st->tokStream.start;
    }
    else {
      dllist_popStart(token)(&st->tokStream, NULL);
      return;
    }
  }
  assert(false);
}
