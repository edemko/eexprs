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

void lexer_addTok(lexer* st, const token* tok) {
  dllistNode_token* node = dllist_insertAfter_token(&st->tokStream, NULL, tok);
  node->here.transparent = false;
}

void lexer_insertBefore(lexer* st, const token* t, dllistNode_token* node) {
  dllistNode_token* new = dllist_insertBefore_token(&st->tokStream, t, node);
  new->here.transparent = false;
}

void lexer_delTok(lexer* st) {
  if (st->tokStream.end != NULL) { token_deinit(&st->tokStream.end->here); }
  dllist_popEnd_token(&st->tokStream, NULL);
}
