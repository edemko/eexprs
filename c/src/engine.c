#include <assert.h>
#include <string.h>

#include "lexer/util.h"
#include "parser/util.h"
#include "shim/common.h"

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
