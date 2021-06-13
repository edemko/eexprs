#include <assert.h>
#include <string.h>

#include "lexer/util.h"
#include "parser/util.h"
#include "shim/common.h"

void parser_init(parser* it) {
  str emptyStr = {.len = 0, .bytes = NULL};
  {
    it->rest = emptyStr;
    it->loc.line = 0;
    it->loc.col = 0;
  }
  {
    dynarr_init(eexprPtr)(&it->eexprStream, 64);
    it->tokStream = dllist_empty(token)();
    it->warnStream = dllist_empty(eexprError)();
    it->errStream = dllist_empty(eexprError)();
    it->fatal.type = EEXPRERR_NOERROR;
  }
  {
    it->discoveredNewline = NEWLINE_NONE;
    it->indent.chr = UCHAR_NULL;
    it->indent.knownMixed = false;
    dynarr_init(openWrap)(&it->wrapStack, 30);
  }
  it->allInput = emptyStr;
  {
    it->lineIndex.len = 1;
    it->lineIndex.cap = 256;
    it->lineIndex.offsets = malloc(sizeof(size_t) * 256);
    checkOom(it->lineIndex.offsets);
    it->lineIndex.offsets[0] = 0;
  }
}

parser parser_newFromFile(const char* filename) {
  parser out;
  parser_init(&out);
  out.allInput = readFile(filename);
  out.rest = out.allInput;
  return out;
}


void parser_del(lexer* it) {
  if (it->lineIndex.offsets != NULL) {
    free(it->lineIndex.offsets);
    it->lineIndex.offsets = NULL;
    it->lineIndex.cap = 0;
    it->lineIndex.len = 0;
  }
  if (it->allInput.bytes != NULL) {
    free(it->allInput.bytes);
    it->allInput.bytes = NULL;
    it->allInput.len = 0;
    // .rest should aliased .allInput
    it->rest.bytes = NULL;
    it->rest.len = 0;
  }
  dynarr_deinit(openWrap)(&it->wrapStack);
  // WARNING I'm assuming there's no owned pointer data in eexprError
  it->fatal.type = EEXPRERR_NOERROR;
  dllist_del(eexprError)(&it->errStream);
  dllist_del(eexprError)(&it->warnStream);

  for (dllistNode(token)* node = it->tokStream.start; node != NULL; node = node->next) {
    token_deinit(&node->here);
  }
  dllist_del(token)(&it->tokStream);

  for (size_t i = 0; i < it->eexprStream.len; ++i) {
    eexpr_deinit(it->eexprStream.data[i]);
    free(it->eexprStream.data[i]);
  }
  dynarr_deinit(eexprPtr)(&it->eexprStream);
}
