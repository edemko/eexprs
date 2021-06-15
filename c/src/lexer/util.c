#include <assert.h>
#include <string.h>

#include "lexer/util.h"
#include "shim/common.h"


void lexer_advance(engine* st, size_t bytes, size_t cols) {
  st->rest.len -= bytes;
  st->rest.bytes += bytes;
  st->loc.col += cols;
  st->loc.byte += bytes;
}
void lexer_incLine(engine* st, size_t bytes) {
  st->rest.len -= bytes;
  st->rest.bytes += bytes;
  st->loc.line += 1;
  st->loc.col = 0;
  st->loc.byte += bytes;
}

void lexer_addTok(engine* st, const eexpr_token* tok) {
  dllistNode_eexpr_token* node = dllist_insertAfter_eexpr_token(&st->tokStream, NULL, tok);
  node->here.transparent = false;
}

void lexer_insertBefore(engine* st, const eexpr_token* t, dllistNode_eexpr_token* node) {
  dllistNode_eexpr_token* new = dllist_insertBefore_eexpr_token(&st->tokStream, t, node);
  new->here.transparent = false;
}

void lexer_delTok(engine* st) {
  if (st->tokStream.end != NULL) { token_deinit(&st->tokStream.end->here); }
  dllist_popEnd_eexpr_token(&st->tokStream, NULL);
}
