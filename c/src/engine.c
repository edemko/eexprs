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
    it->tokStream = NULL;
    it->tokStream_end = NULL;
    it->warnStream = NULL;
    it->warnStream_end = NULL;
    it->errStream = NULL;
    it->errStream_end = NULL;
    it->fatal = NULL;
  }
  {
    it->eexprStream = malloc(sizeof(eexprStream) + 128 * sizeof(eexpr));
    checkOom(it->eexprStream);
    it->eexprStream->cap = 128;
    it->eexprStream->len = 0;
  }
  {
    it->discoveredNewline = NEWLINE_NONE;
    it->indent.chr = UCHAR_NULL;
    it->indent.knownMixed = false;
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


void parser_del(lexer* st) {
  if (st->lineIndex.offsets != NULL) {
    free(st->lineIndex.offsets);
    st->lineIndex.offsets = NULL;
    st->lineIndex.cap = 0;
    st->lineIndex.len = 0;
  }
  if (st->allInput.bytes != NULL) {
    free(st->allInput.bytes);
    st->allInput.bytes = NULL;
    st->allInput.len = 0;
    // .rest should aliased .allInput
    st->rest.bytes = NULL;
    st->rest.len = 0;
  }
  if (st->fatal != NULL) {
    free(st->fatal);
    st->fatal = NULL;
  }
  if (st->errStream != NULL) {
    lexErrStream_del(st->errStream);
    st->errStream = NULL;
    st->errStream_end = NULL;
  }
  if (st->warnStream != NULL) {
    lexErrStream_del(st->warnStream);
    st->warnStream = NULL;
    st->warnStream_end = NULL;
  }
  if (st->eexprStream != NULL) {
    eexprStream_del(st->eexprStream);
    free(st->eexprStream);
    st->eexprStream = NULL;
  }
  if (st->tokStream != NULL) {
    tokenStream_del(st->tokStream);
    st->tokStream = NULL;
    st->tokStream_end = NULL;
  }
}

void eexpr_deinit(eexpr* expr) {
  if (expr == NULL) { return; }
  switch (expr->type) {
    case EEXPR_NUMBER: {
      free(expr->as.number.mantissa.buf);
      free(expr->as.number.exponent.buf);
    }; break;
    // TODO free more owned pointers
  }
}
