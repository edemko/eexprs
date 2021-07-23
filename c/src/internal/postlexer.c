#include <assert.h>

#include "common.h"
#include "engine.h"
#include "parameters.h"

#define TYPE size_t
#include "dynarr.h"

static
dllistNode_eexpr_token* getPrev(dllistNode_eexpr_token* tok) {
  if (tok == NULL) { return NULL; }
  do {
    tok = tok->prev;
  } while (tok != NULL && tok->here.transparent);
  return tok;
}

static
dllistNode_eexpr_token* getNext(dllistNode_eexpr_token* tok) {
  if (tok == NULL) { return NULL; }
  do {
    tok = tok->next;
  } while (tok != NULL && tok->here.transparent);
  return tok;
}

/*
  `^(newline | start-of-file) end-of-file --> error`
*/
static
void ensureTrailingNewline(engine* st) {
  dllistNode_eexpr_token* ultimate = st->tokStream.end;
  assert(ultimate != NULL);
  assert(ultimate->here.type == EEXPR_TOK_EOF);
  dllistNode_eexpr_token* penultimate = getPrev(ultimate);
  if ( penultimate != NULL
    && penultimate->here.type != EEXPR_TOK_UNKNOWN_NEWLINE
     ) {
    eexpr_error err = {.loc = ultimate->here.loc, .type = EEXPR_ERR_NO_TRAILING_NEWLINE};
    dllist_insertAfter_eexpr_error(&st->errStream, NULL, &err);
  }
}

/*
  `space? comment? end-of-line --> end-of-line`
  `space line-continue -> line-continue`
  `line-continue space -> space`
*/
static
void ignoreTrailingStuff(engine* st) {
  for (dllistNode_eexpr_token* strm = st->tokStream.start; strm != NULL; strm = strm->next) {
    if (strm->here.transparent) { continue; }
    // ignore (and create errors for) whitespace at the end of lines
    if (strm->here.type == EEXPR_TOK_UNKNOWN_SPACE) {
      assert(strm->next != NULL);
      if ( strm->next->here.type == EEXPR_TOK_UNKNOWN_NEWLINE
        || strm->next->here.type == EEXPR_TOK_EOF
         ) {
        strm->here.transparent = true;
        eexpr_error err = {.loc = strm->here.loc, .type = EEXPR_ERR_TRAILING_SPACE};
        dllist_insertAfter_eexpr_error(&st->errStream, NULL, &err);
      }
      else if (strm->here.as.unknownSpace.type == EEXPR_WSLINECONTINUE) {
        dllistNode_eexpr_token* prev = getPrev(strm);
        if (prev->here.type == EEXPR_TOK_UNKNOWN_SPACE) {
          prev->here.transparent = true;
        }
        dllistNode_eexpr_token* next = getNext(strm);
        if (next->here.type == EEXPR_TOK_UNKNOWN_SPACE) {
          strm->here.transparent = true;
        }
      }
    }
    // ignore comments and any whitespace that precedes them
    else if (strm->here.type == EEXPR_TOK_COMMENT) {
      strm->here.transparent = true;
      if (strm->prev != NULL && strm->prev->here.type == EEXPR_TOK_UNKNOWN_SPACE) {
        strm->prev->here.transparent = true;
      }
    }
  }
}

/*
  `newline end-of-line --> end-of-line`
*/
static
void ignoreBlankLines(engine* st) {
  for (dllistNode_eexpr_token* strm = st->tokStream.start; strm != NULL; strm = strm->next) {
    if (strm->here.transparent) { continue; }
    if (strm->here.type == EEXPR_TOK_UNKNOWN_NEWLINE) {
      dllistNode_eexpr_token* next = getNext(strm);
      if ( next->here.type == EEXPR_TOK_UNKNOWN_NEWLINE
        || next->here.type == EEXPR_TOK_EOF
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
  `space_1 unknown-dot ^(space | end-of-line)_2 --> \1 predot \2`
  `unknown-dot --> error`
*/
static
void disambiguateDots(engine* st) {
  for (dllistNode_eexpr_token* strm = st->tokStream.start; strm != NULL; strm = strm->next) {
    if (strm->here.transparent) { continue; }
    if (strm->here.type == EEXPR_TOK_UNKNOWN_DOT) {
      dllistNode_eexpr_token* prev = getPrev(strm);
      bool spaceBefore = prev            == NULL
                      || prev->here.type == EEXPR_TOK_NEWLINE
                      || prev->here.type == EEXPR_TOK_SPACE
                       ;
      bool trueSpaceBefore = prev != NULL && prev->here.type == EEXPR_TOK_SPACE;
      dllistNode_eexpr_token* next = getNext(strm);
      bool spaceAfter = next->here.type == EEXPR_TOK_EOF
                     || next->here.type == EEXPR_TOK_NEWLINE
                     || next->here.type == EEXPR_TOK_SPACE
                      ;
      if (!spaceBefore && !spaceAfter) {
        strm->here.type = EEXPR_TOK_CHAIN;
      }
      else if (trueSpaceBefore && !spaceAfter) {
        strm->here.type = EEXPR_TOK_PREDOT;
      }
      else {
        eexpr_error err = {.loc = strm->here.loc, .type = EEXPR_ERR_BAD_DOT};
        dllist_insertAfter_eexpr_error(&st->errStream, NULL, &err);
      }
    }
  }
}

/*
  `unknown-colon eol space(n)?_1 --> open-indent(n || 0)`
  `^(space | start-of-line | unknown-dot) open-indent --> space open-indent`
  `unknown-colon ^end-of-line --> colon`
*/
static
void disambiguateColons(engine* st) {
  for (dllistNode_eexpr_token* strm = st->tokStream.start; strm != NULL; strm = strm->next) {
    if (strm->here.transparent) { continue; }
    if (strm->here.type == EEXPR_TOK_UNKNOWN_COLON) {
      dllistNode_eexpr_token* next = getNext(strm);
      if (next->here.type == EEXPR_TOK_UNKNOWN_NEWLINE) {
        dllistNode_eexpr_token* ws = getNext(next);
        strm->here.type = EEXPR_TOK_INDENT;
        strm->here.as.indent.depth
          = ws->here.type == EEXPR_TOK_UNKNOWN_SPACE ? ws->here.as.unknownSpace.size : 0;
        next->here.transparent = true;
        if (ws->here.type == EEXPR_TOK_UNKNOWN_SPACE) {
          ws->here.transparent = true;
        }
        // insert space before indented block
        dllistNode_eexpr_token* prev = getPrev(strm);
        if ( prev->here.type != EEXPR_TOK_UNKNOWN_NEWLINE
          && prev->here.type != EEXPR_TOK_UNKNOWN_SPACE
          && prev->here.type != EEXPR_TOK_UNKNOWN_DOT
          ) {
          eexpr_token synthSpace =
            { .loc = { .start = strm->here.loc.start, .end = strm->here.loc.start }
            , .type = EEXPR_TOK_SPACE
            , .transparent = false
            };
          dllist_insertBefore_eexpr_token(&st->tokStream, &synthSpace, strm);
        }
      }
      else if (next->here.type == EEXPR_TOK_EOF) {
        strm->here.type = EEXPR_TOK_INDENT;
        strm->here.as.indent.depth = 0;
      }
      else {
        strm->here.type = EEXPR_TOK_COLON;
      }
    }
    else if (strm->here.type == EEXPR_TOK_WRAP && strm->here.as.wrap.isOpen) {
      dllistNode_eexpr_token* newline = getNext(strm);
      if (newline->here.type == EEXPR_TOK_UNKNOWN_NEWLINE) {
        dllistNode_eexpr_token* ws = getNext(newline);
        newline->here.type = EEXPR_TOK_INDENT;
        newline->here.as.indent.depth
          = ws->here.type == EEXPR_TOK_UNKNOWN_SPACE ? ws->here.as.unknownSpace.size : 0;
        if (ws->here.type == EEXPR_TOK_UNKNOWN_SPACE) {
          ws->here.transparent = true;
        }
      }
    }
  }
}

static
size_t indentState_peek(const dynarr_size_t* st) {
  return st->len == 0 ? 0 : st->data[st->len-1];
}
static
size_t indentState_pop(dynarr_size_t* st) {
  return st->len == 0 ? 0 : st->data[--st->len];
}

static
bool insertDedents(engine* st, dynarr_size_t* depths, dllistNode_eexpr_token* endOfLine) {
  size_t newDepth;
  dllistNode_eexpr_token* insertPoint;
  if (endOfLine->here.type == EEXPR_TOK_UNKNOWN_NEWLINE) {
    dllistNode_eexpr_token* maybeSpace = getNext(endOfLine);
    endOfLine->here.transparent = true;
    if (maybeSpace->here.type == EEXPR_TOK_UNKNOWN_SPACE) {
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
  else if (endOfLine->here.type == EEXPR_TOK_EOF) {
    newDepth = 0;
    insertPoint = endOfLine;
  }
  else { assert(false); }
  eexpr_loc loc =
    { .start = {.line = insertPoint->here.loc.start.line, .col = 0}
    , .end = insertPoint->here.loc.start
    };
  assert(newDepth <= indentState_peek(depths)); // this should have been handled above, before the newline and whitespace was ignored
  while (true) {
    size_t depth = indentState_peek(depths);
    if (newDepth < depth) {
      eexpr_token tok = {.loc = loc, .type = EEXPR_TOK_WRAP, .as.wrap = {.type = EEXPR_WRAP_BLOCK, .isOpen = false}};
      lexer_insertBefore(st, &tok, insertPoint);
      indentState_pop(depths);
    }
    else if (newDepth == depth) {
      if (insertPoint->here.type == EEXPR_TOK_WRAP && !insertPoint->here.as.wrap.isOpen) {
        // do nothing: supress newline between dedent and close wrap
      }
      else if (insertPoint->here.type == EEXPR_TOK_EOF) {
        // do nothing: no need to insert a newline when we're at the end of the file
      }
      else {
        eexpr_token tok = {.loc = loc, .type = EEXPR_TOK_NEWLINE};
        lexer_insertBefore(st, &tok, insertPoint);
      }
      return true;
    }
    else {
      eexpr_error err = {.loc = loc, .type = EEXPR_ERR_OFFSIDES};
      dllist_insertAfter_eexpr_error(&st->errStream, NULL, &err);
      return false;
    }
  }
}
/*
Indentation is done on a stack.
Indentation is pushed when a colon or open wrap immediately precedes a newline.
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
  supress newline before close wrap (which includes dedents, and in a way, end-of-file since that matches start of file).

Note that whereas the colon is consumed by the indentation, the open wrap is not.
*/
// Detecting open indents is done by `disambiguateColons`, even though the name implies it's only worried about colons.
static
bool detectIndentation(engine* st) {
  bool success = false;
  dynarr_size_t depths; dynarr_init_size_t(&depths, 30);
  for (dllistNode_eexpr_token* strm = st->tokStream.start; strm != NULL; strm = strm->next) {
    if (strm->here.transparent) { continue; }
    if (strm->here.type == EEXPR_TOK_INDENT) {
      dllistNode_eexpr_token* next = getNext(strm);
      eexpr_loc loc =
        { .start = {.line = next->here.loc.start.line, .col = 0}
        , .end = next->here.loc.start
        };
      size_t depth = strm->here.as.indent.depth;
      size_t depth0 = indentState_peek(&depths);
      if (depth > depth0) {
        dynarr_push_size_t(&depths, &depth);
        eexpr_token tok = {.loc = loc, .type = EEXPR_TOK_WRAP, .as.wrap = {.type = EEXPR_WRAP_BLOCK, .isOpen = true}};
        lexer_insertBefore(st, &tok, next);
        strm->here.transparent = true;
        success = true;
      }
      else {
        eexpr_error err = {.loc = loc, .type = EEXPR_ERR_SHALLOW_INDENT};
        dllist_insertAfter_eexpr_error(&st->errStream, NULL, &err);
        success = false;
      }
    }
    else if ( strm->here.type == EEXPR_TOK_UNKNOWN_NEWLINE
           || strm->here.type == EEXPR_TOK_EOF
            ) {
      success = insertDedents(st, &depths, strm);
    }
  }
  dynarr_deinit_size_t(&depths);
  return success;
}

// This really just checks that newlines and inline space have all been handled.
static
void disambiguateSpaces(engine* st) {
  for (dllistNode_eexpr_token* strm = st->tokStream.start; strm != NULL; strm = strm->next) {
    if (strm->here.transparent) { continue; }
    // all newlines should already have been handled
    assert (strm->here.type != EEXPR_TOK_UNKNOWN_NEWLINE);
    if (strm->here.type == EEXPR_TOK_UNKNOWN_SPACE) {
      // we (should already) know this is inline space
      strm->here.type = EEXPR_TOK_SPACE;
      // we should already have merged adjacent spaces
      {
        dllistNode_eexpr_token* prev = getPrev(strm);
        assert(prev->here.type != EEXPR_TOK_SPACE);
      }
      // space at start of a line should already have been handled
      {
        dllistNode_eexpr_token* prev = getPrev(strm);
        assert(prev != NULL);
        assert(prev->here.type != EEXPR_TOK_NEWLINE);
      }
      // space at end of line should already have been handled
      {
        dllistNode_eexpr_token* next = getPrev(strm);
        assert(next->here.type != EEXPR_TOK_NEWLINE);
        assert(next->here.type != EEXPR_TOK_EOF);
      }
    }
  }
}

/*
  `(wrap.open | string.open | string.middle)^1 space --> \1`
  `space (wrap.close | string.close | string.middle)^1 --> \1`
*/
static
void ignoreWrappedSpaces(engine* st) {
  for (dllistNode_eexpr_token* strm = st->tokStream.start; strm != NULL; strm = strm->next) {
    if (strm->here.transparent) { continue; }
    if ( (strm->here.type == EEXPR_TOK_WRAP && strm->here.as.wrap.isOpen)
      || (strm->here.type == EEXPR_TOK_STRING
        && ( strm->here.as.string.splice == EEXPR_STROPEN
          || strm->here.as.string.splice == EEXPR_STRMIDDLE
           )
         )
       ) {
      dllistNode_eexpr_token* next = getNext(strm);
      if (next != NULL && next->here.type == EEXPR_TOK_SPACE) {
        next->here.transparent = true;
      }
    }
    else if (strm->here.type == EEXPR_TOK_SPACE) {
      dllistNode_eexpr_token* next = getNext(strm);
      if ( (next->here.type == EEXPR_TOK_WRAP && !next->here.as.wrap.isOpen)
        || (next->here.type == EEXPR_TOK_STRING
          && ( next->here.as.string.splice == EEXPR_STRCLOSE
            || next->here.as.string.splice == EEXPR_STRMIDDLE
             )
           )
         ) {
        strm->here.transparent = true;
      }
    }
  }
}

/*
  `(chain | ellipsis) (ellipsis | chain) --> error`
  `number chain --> error`
  `(number | symbol) (number | symbol) --> error`
  `string.(close | plain) string.(open | plain) --> error`
*/
static
void detectCramming(engine* st) {
  for (dllistNode_eexpr_token* strm = st->tokStream.start; strm != NULL; strm = strm->next) {
    if (strm->here.transparent) { continue; }
    if (strm->here.type == EEXPR_TOK_EOF) { continue; }
    eexpr_tokenType hereType = strm->here.type;
    bool hereIsDotLike = hereType == EEXPR_TOK_ELLIPSIS || hereType == EEXPR_TOK_CHAIN || hereType == EEXPR_TOK_PREDOT;
    dllistNode_eexpr_token* next = getNext(strm);
    eexpr_tokenType nextType = next->here.type;
    bool nextIsDotLike = nextType == EEXPR_TOK_ELLIPSIS || nextType == EEXPR_TOK_CHAIN || nextType == EEXPR_TOK_PREDOT;
    eexpr_loc loc = {.start = strm->here.loc.start, .end = next->here.loc.end};
    eexpr_error err = {.loc = loc, .type = EEXPR_ERR_CRAMMED_TOKENS};
    if (hereIsDotLike && nextIsDotLike) {
      dllist_insertAfter_eexpr_error(&st->errStream, NULL, &err);
    }
    else if (hereType == EEXPR_TOK_NUMBER && nextType == EEXPR_TOK_CHAIN) {
      dllist_insertAfter_eexpr_error(&st->errStream, NULL, &err);
    }
    else if (hereType == EEXPR_TOK_SYMBOL || hereType == EEXPR_TOK_NUMBER) {
      if (nextType == EEXPR_TOK_SYMBOL || nextType == EEXPR_TOK_NUMBER) {
        dllist_insertAfter_eexpr_error(&st->errStream, NULL, &err);
      }
    }
    else if (hereType == EEXPR_TOK_STRING && nextType == EEXPR_TOK_STRING) {
      bool hereStringClosed = strm->here.as.string.splice == EEXPR_STRPLAIN || strm->here.as.string.splice == EEXPR_STRCLOSE;
      bool nextStringOpen = strm->here.as.string.splice == EEXPR_STRPLAIN || strm->here.as.string.splice == EEXPR_STROPEN;
      if (hereStringClosed && nextStringOpen) {
        dllist_insertAfter_eexpr_error(&st->errStream, NULL, &err);
      }
    }
  }
}

void engine_cookLex(engine* st) {
  ensureTrailingNewline(st);
  ignoreTrailingStuff(st);
  ignoreBlankLines(st);
  disambiguateColons(st);
  if (!detectIndentation(st)) { return; }
  disambiguateSpaces(st);
  ignoreWrappedSpaces(st);
  disambiguateDots(st);
  detectCramming(st);
  // TODO detect mixed indentation
  // TODO detect mixed newlines
  // TODO create error if file starts with indent
}
