#include <assert.h>

#include "lexer/util.h"
#include "parameters.h"
#include "shim/common.h"


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

/*
  `^(newline | start-of-file) end-of-file --> error`
*/
void ensureTrailingNewline(lexer* st) {
  tokenStream* ultimate = st->tokStream_end;
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

/*
  `space? comment? end-of-line --> end-of-line`
  `space line-continue -> line-continue`
  `line-continue space -> space`
*/
void ignoreTrailingStuff(lexer* st) {
  for (tokenStream* strm = st->tokStream; strm != NULL; strm = strm->next) {
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
      else if (strm->here.as.unknownSpace.chr == escapeLeader) {
        tokenStream* prev = getPrev(strm);
        if (prev->here.type == TOK_UNKNOWN_SPACE) {
          prev->here.transparent = true;
        }
        tokenStream* next = getNext(strm);
        if (next->here.type == TOK_UNKNOWN_SPACE) {
          strm->here.transparent = true;
        }
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

/*
  `newline end-of-line --> end-of-line`
*/
void ignoreBlankLines(lexer* st) {
  for (tokenStream* strm = st->tokStream; strm != NULL; strm = strm->next) {
    if (strm->here.transparent) { continue; }
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

/*
  `^(space | start-of-line)_1 unknown-dot ^(space | end-of-line)_2 --> \1 chain \2`
  `space_1 unknown-dot ^(space | end-of-line)_2 --> \1 synthfix \2`
  `unknown-dot --> error`
*/
void disambiguateDots(lexer* st) {
  for (tokenStream* strm = st->tokStream; strm != NULL; strm = strm->next) {
    if (strm->here.transparent) { continue; }
    if (strm->here.type == TOK_UNKNOWN_DOT) {
      tokenStream* prev = getPrev(strm);
      bool spaceBefore = prev            == NULL
                      || prev->here.type == TOK_NEWLINE
                      || prev->here.type == TOK_SPACE
                       ;
      bool trueSpaceBefore = prev != NULL && prev->here.type == TOK_SPACE;
      tokenStream* next = getNext(strm);
      bool spaceAfter = next->here.type == TOK_EOF
                     || next->here.type == TOK_NEWLINE
                     || next->here.type == TOK_SPACE
                      ;
      if (!spaceBefore && !spaceAfter) {
        strm->here.type = TOK_CHAIN;
      }
      else if (trueSpaceBefore && !spaceAfter) {
        strm->here.type = TOK_SYNTHFIX;
      }
      else {
        lexError err = {.loc = strm->here.loc, .type = LEXERR_BAD_DOT};
        lexer_addErr(st, &err);
      }
    }
  }
}

/*
  `unknown-colon end-of-line whitespace(n)?_1 --> open-indent(n || 0) end-of-line \1`
  `unknown-colon ^end-of-line --> colon
*/
void disambiguateColons(lexer* st) {
  for (tokenStream* strm = st->tokStream; strm != NULL; strm = strm->next) {
    if (strm->here.transparent) { continue; }
    if (strm->here.type == TOK_UNKNOWN_COLON) {
      tokenStream* next = getNext(strm);
      if (next->here.type == TOK_UNKNOWN_NEWLINE) {
        tokenStream* ws = getNext(next);
        strm->here.type = TOK_OPEN_INDENT;
        strm->here.as.indent.depth
          = ws->here.type == TOK_UNKNOWN_SPACE ? ws->here.as.unknownSpace.size : 0;
        next->here.transparent = true;
        if (ws->here.type == TOK_UNKNOWN_SPACE) {
          ws->here.transparent = true;
        }
      }
      else if (next->here.type == TOK_EOF) {
        strm->here.type = TOK_OPEN_INDENT;
        strm->here.as.indent.depth = 0;
      }
      else {
        strm->here.type = TOK_COLON;
      }
    }
    else if (strm->here.type == TOK_WRAPPER && strm->here.as.wrapper.isOpen) {
      tokenStream* newline = getNext(strm);
      if (newline->here.type == TOK_UNKNOWN_NEWLINE) {
        tokenStream* ws = getNext(newline);
        newline->here.type = TOK_OPEN_INDENT;
        newline->here.as.indent.depth
          = ws->here.type == TOK_UNKNOWN_SPACE ? ws->here.as.unknownSpace.size : 0;
        if (ws->here.type == TOK_UNKNOWN_SPACE) {
          ws->here.transparent = true;
        }
      }
    }
  }
}

typedef struct indentState {
  size_t cap;
  size_t len;
  size_t* depths;
} indentState;
indentState indentState_new() {
  indentState st = {.cap = 128, .len = 0};
  size_t* depths = malloc(st.cap * sizeof(size_t));
  checkOom(depths);
  st.depths = depths;
  return st;
}
void indentState_push(indentState* st, size_t depth) {
  if (st->len == st->cap) {
    st->cap *= 2;
    st->depths = realloc(st->depths, st->cap);
    checkOom(st->depths);
  }
  st->depths[st->len++] = depth;
}
size_t indentState_peek(const indentState* st) {
  return st->len == 0 ? 0 : st->depths[st->len-1];
}
size_t indentState_pop(indentState* st) {
  return st->len == 0 ? 0 : st->depths[--st->len];
}
bool insertDedents(lexer* st, indentState* depths, tokenStream* endOfLine) {
  size_t newDepth;
  tokenStream* insertPoint;
  if (endOfLine->here.type == TOK_UNKNOWN_NEWLINE) {
    tokenStream* maybeSpace = getNext(endOfLine);
    endOfLine->here.transparent = true;
    if (maybeSpace->here.type == TOK_UNKNOWN_SPACE) {
      newDepth = maybeSpace->here.as.unknownSpace.size;
      if (newDepth > indentState_peek(depths)) {
        // keep the whitespace, but not the newline
        // no change to the depth stack
        return true;
      }
      maybeSpace->here.transparent = true;
      insertPoint = getNext(maybeSpace);
    }
    else {
      newDepth = 0;
      insertPoint = maybeSpace;
    }
  }
  else if (endOfLine->here.type == TOK_EOF) {
    newDepth = 0;
    insertPoint = endOfLine;
  }
  else { assert(false); }
  fileloc loc =
    { .start = {.line = insertPoint->here.loc.start.line, .col = 0}
    , .end = insertPoint->here.loc.start
    };
  assert(newDepth <= indentState_peek(depths)); // this should have been handled above, before the newline and whitespace was ignored
  while (true) {
    size_t depth = indentState_peek(depths);
    if (newDepth < depth) {
      token tok = {.loc = loc, .type = TOK_WRAPPER, .as.wrapper = {.chr = '\n', .isOpen = false}};
      lexer_insertBefore(st, &tok, insertPoint);
      indentState_pop(depths);
    }
    else if (newDepth == depth) {
      if (insertPoint->here.type == TOK_WRAPPER && !insertPoint->here.as.wrapper.isOpen) {
        // do nothing: supress newline between dedent and close wrapper
      }
      else if (insertPoint->here.type == TOK_EOF) {
        // do nothing: no need to insert a newline when we're at the end of the file
      }
      else {
        token tok = {.loc = loc, .type = TOK_NEWLINE};
        lexer_insertBefore(st, &tok, insertPoint);
      }
      return true;
    }
    else {
      lexError err = {.loc = loc, .type = LEXERR_OFFSIDES};
      lexer_addErr(st, &err);
      return false;
    }
  }
}
/*
Indentation is done on a stack.
Indentation is pushed when a colon or open wrapper immediately precedes a newline.
A newline followed by more space than is currently on the stack is treated as inline space.
A newline followed by exactly as much space as is currently on the stack is considered a newline.
A newline followed by fewer spaces than is on the stack is treated as one or more dedents:
  each position on the stack greater than the number of spaces is another dedent, but
  the number of spaces must be present somewhere on the stack, or else that is an "offsides" error.

Usually, when a sequence of dedents is generated, it is immediately followed by a newline,
  because dedenting must have matched another dedent level on the stack.
However, if the dedent sequence is generated by the end-of-file or a close paren/bracket/brace,
  that newline is respectively redundant, or unwanted.
Supressing newline before close paren/bracket/brace is consistent with a more general rule:
  supress newline before close wrapper (which includes dedents, and in a way, end-of-file since that matches start of file).

Note that whereas the colon is consumed by the indentation, the open wrapper is not.
*/
// Detecting open indents is done by `disambiguateColons`, even though the name implies it's only worried about colons.
bool detectIndentation(lexer* st) {
  bool success = false;
  indentState depths = indentState_new();
  for (tokenStream* strm = st->tokStream; strm != NULL; strm = strm->next) {
    if (strm->here.transparent) { continue; }
    if (strm->here.type == TOK_OPEN_INDENT) {
      tokenStream* next = getNext(strm);
      fileloc loc =
        { .start = {.line = next->here.loc.start.line, .col = 0}
        , .end = next->here.loc.start
        };
      size_t depth = strm->here.as.indent.depth;
      size_t depth0 = indentState_peek(&depths);
      if (depth > depth0) {
        indentState_push(&depths, depth);
        token tok = {.loc = loc, .type = TOK_WRAPPER, .as.wrapper = {.chr = '\n', .isOpen = true}};
        lexer_insertBefore(st, &tok, next);
        strm->here.transparent = true;
        success = true;
      }
      else {
        lexError err = {.loc = loc, .type = LEXERR_SHALLOW_INDENT};
        lexer_addErr(st, &err);
        success = false;
      }
    }
    else if ( strm->here.type == TOK_UNKNOWN_NEWLINE
           || strm->here.type == TOK_EOF
            ) {
      success = insertDedents(st, &depths, strm);
    }
  }
  free(depths.depths);
  return success;
}

// This really just checks that newlines and inline space have all been handled.
void disambiguateSpaces(lexer* st) {
  for (tokenStream* strm = st->tokStream; strm != NULL; strm = strm->next) {
    if (strm->here.transparent) { continue; }
    // all newlines should already have been handled
    assert (strm->here.type != TOK_UNKNOWN_NEWLINE);
    if (strm->here.type == TOK_UNKNOWN_SPACE) {
      // we (should already) know this is inline space
      strm->here.type = TOK_SPACE;
      // we should already have merged adjacent spaces
      {
        tokenStream* prev = getPrev(strm);
        assert(prev->here.type != TOK_SPACE);
      }
      // space at start of a line should already have been handled
      {
        tokenStream* prev = getPrev(strm);
        assert(prev != NULL);
        assert(prev->here.type != TOK_NEWLINE);
      }
      // space at end of line should already have been handled
      {
        tokenStream* next = getPrev(strm);
        assert(next->here.type != TOK_NEWLINE);
        assert(next->here.type != TOK_EOF);
      }
    }
  }
}

/*
  `(chain | ellipsis | number) (chain | ellipsis) --> error`
  `(number | symbol) (number | symbol) --> error`
*/
void detectCramming(lexer* st) {
  for (tokenStream* strm = st->tokStream; strm != NULL; strm = strm->next) {
    if (strm->here.transparent) { continue; }
    if (strm->here.type == TOK_EOF) { continue; }
    enum tokenType hereType = strm->here.type;
    bool hereIsDotLike = hereType == TOK_ELLIPSIS || hereType == TOK_CHAIN || hereType == TOK_SYNTHFIX;
    tokenStream* next = getNext(strm);
    enum tokenType nextType = next->here.type;
    bool nextIsDotLike = nextType == TOK_ELLIPSIS || nextType == TOK_CHAIN || nextType == TOK_SYNTHFIX;
    fileloc loc = {.start = strm->here.loc.start, .end = next->here.loc.end};
    lexError err = {.loc = loc, .type = LEXERR_CRAMMED_TOKENS};
    if (hereIsDotLike && nextIsDotLike) {
      lexer_addErr(st, &err);
    }
    else if (hereType == TOK_NUMBER && nextIsDotLike) {
      lexer_addErr(st, &err);
    }
    else if (hereType == TOK_SYMBOL || hereType == TOK_NUMBER) {
      if (nextType == TOK_SYMBOL || nextType == TOK_NUMBER) {
        lexer_addErr(st, &err);
      }
    }
  }
}

void lexer_cook(lexer* st) {
  ensureTrailingNewline(st);
  ignoreTrailingStuff(st);
  ignoreBlankLines(st);
  disambiguateColons(st);
  if (!detectIndentation(st)) { return; }
  disambiguateSpaces(st);
  disambiguateDots(st);
  detectCramming(st);
  // TODO detect mixed indentation
  // TODO create error if file starts with indent
}
