#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "engine.h"


//////////////////////////////////// General Functions ////////////////////////////////////

static
void engine_init(engine* it) {
  str emptyStr = {.len = 0, .bytes = NULL};
  {
    it->rest = emptyStr;
    it->loc.line = 0;
    it->loc.col = 0;
    it->loc.byte = 0;
  }
  {
    dynarr_init_eexpr_p(&it->eexprStream, 64);
    it->tokStream = dllist_empty_eexpr_token();
    it->errStream = dllist_empty_eexpr_error();
    it->fatal.type = EEXPRERR_NOERROR;
  }
  {
    it->discoveredNewline = NEWLINE_NONE;
    it->indent.chr = UCHAR_NULL;
    it->indent.knownMixed = false;
    dynarr_init_openWrap(&it->wrapStack, 30);
  }
}

engine engine_newFromStrn(size_t n, uint8_t* input) {
  engine out;
  engine_init(&out);
  out.rest.len = n;
  out.rest.bytes = input;
  return out;
}


void engine_deinit(engine* it) {
  // .rest should aliased another string anyway
  it->rest.bytes = NULL;
  it->rest.len = 0;
  dynarr_deinit_openWrap(&it->wrapStack);
  // WARNING I'm assuming there's no owned pointer data in error
  it->fatal.type = EEXPRERR_NOERROR;
  dllist_del_eexpr_error(&it->errStream);

  for (dllistNode_eexpr_token* node = it->tokStream.start; node != NULL; node = node->next) {
    token_deinit(&node->here);
  }
  dllist_del_eexpr_token(&it->tokStream);

  for (size_t i = 0; i < it->eexprStream.len; ++i) {
    eexpr_deinit(it->eexprStream.data[i]);
    free(it->eexprStream.data[i]);
  }
  dynarr_deinit_eexpr_p(&it->eexprStream);
}


//////////////////////////////////// Lexer/Postlexer Helper Functions ////////////////////////////////////

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


//////////////////////////////////// Parser Helper Functions ////////////////////////////////////

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
