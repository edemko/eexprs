#include "parser/util.h"

#include <assert.h>
#include <stdlib.h>

#include "shim/common.h"


eexpr_token* parser_peek(engine* st) {
  dllistNode_eexpr_token* node = st->tokStream.start;
  while (node != NULL) {
    if (node->here.transparent) {
      token_deinit(&node->here);
      dllist_popStart_eexpr_token(&st->tokStream, NULL);
      node = st->tokStream.start;
    }
    else {
      return &node->here;
    }
  }
  return NULL;
}

void parser_pop(engine* st) {
  dllistNode_eexpr_token* node = st->tokStream.start;
  while (node != NULL) {
    if (node->here.transparent) {
      token_deinit(&node->here);
      dllist_popStart_eexpr_token(&st->tokStream, NULL);
      node = st->tokStream.start;
    }
    else {
      dllist_popStart_eexpr_token(&st->tokStream, NULL);
      return;
    }
  }
  assert(false);
}
