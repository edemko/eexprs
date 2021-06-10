#include "lexer.h"

#include <assert.h>


tokenStream* getPrev(tokenStream* tok) {
  if (tok == NULL) { return NULL; }
  do {
    tok = tok->prev;
  } while (tok != NULL && tok->here.transparent);
  return tok;
}

tokenStream* getNext(tokenStream* tok) {
  if (tok == NULL) { return NULL; }
  do {
    tok = tok->next;
  } while (tok != NULL && tok->here.transparent);
  return tok;
}

void ignoreTrailingStuff(lexer* st) {
  for (tokenStream* strm = st->outStream; strm != NULL; strm = strm->next) {
    if (strm->here.transparent) { continue; }
    // ignore (and create errors for) whitespace at the end of lines
    if (strm->here.type == TOK_UNKNOWN_SPACE) {
      if ( strm->next->here.type == TOK_UNKNOWN_NEWLINE
        || strm->next->here.type == TOK_EOF
         ) {
        strm->here.transparent = true;
        lexError err = {.loc = strm->here.loc, .type = LEXERR_TRAILING_SPACE};
        lexer_addErr(st, &err);
      }
    }
    // ignore comments and any whitespace that precedes them
    else if (strm->here.type == TOK_COMMENT) {
      strm->here.transparent = true;
      if (strm->prev != NULL && strm->prev->here.type == TOK_UNKNOWN_SPACE) {
        strm->prev->here.transparent = true;
      }
    }
  }
}

void ensureTrailingNewline(lexer* st) {
  tokenStream* ultimate = st->outStream_end;
  assert(ultimate != NULL);
  assert(ultimate->here.type == TOK_EOF);
  tokenStream* penultimate = getPrev(ultimate);
  if ( penultimate != NULL
    && penultimate->here.type != TOK_UNKNOWN_NEWLINE
     ) {
    lexError err = {.loc = ultimate->here.loc, .type = LEXERR_NO_TRAILING_NEWLINE};
    lexer_addErr(st, &err);
  }
}

void ignoreBlankLines(lexer* st) {
  for (tokenStream* strm = st->outStream; strm != NULL; strm = strm->next) {
    if (strm->here.type == TOK_UNKNOWN_NEWLINE) {
      tokenStream* next = getNext(strm);
      if ( next->here.type == TOK_UNKNOWN_NEWLINE
        || next->here.type == TOK_EOF
         ) {
        strm->here.transparent = true;
      }
      else if (getPrev(strm) == NULL) {
        strm->here.transparent = true;
      }
    }
  }
}

void lexer_cook(lexer* st) {
  ignoreTrailingStuff(st);
  ensureTrailingNewline(st);
  ignoreBlankLines(st);
  // TODO detect indentation
  // TODO detect crammed atoms
  // TODO recognizeDots
  // TODO recognizeSpaces
  // TODO map ensureSensitive
}
