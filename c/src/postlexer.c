#include <assert.h>

#include "lexer/util.h"
#include "parameters.h"
#include "shim/common.h"


dllistNode_token* getPrev(dllistNode_token* tok) {
  if (tok == NULL) { return NULL; }
  do {
    tok = tok->prev;
  } while (tok != NULL && tok->here.transparent);
  return tok;
}

dllistNode_token* getNext(dllistNode_token* tok) {
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
  dllistNode_token* ultimate = st->tokStream.end;
  assert(ultimate != NULL);
  assert(ultimate->here.type == TOK_EOF);
  dllistNode_token* penultimate = getPrev(ultimate);
  if ( penultimate != NULL
    && penultimate->here.type != TOK_UNKNOWN_NEWLINE
     ) {
    eexprError err = {.loc = ultimate->here.loc, .type = EEXPRERR_NO_TRAILING_NEWLINE};
    dllist_insertAfter_eexprError(&st->errStream, NULL, &err);
  }
}

/*
  `space? comment? end-of-line --> end-of-line`
  `space line-continue -> line-continue`
  `line-continue space -> space`
*/
void ignoreTrailingStuff(lexer* st) {
  for (dllistNode_token* strm = st->tokStream.start; strm != NULL; strm = strm->next) {
    if (strm->here.transparent) { continue; }
    // ignore (and create errors for) whitespace at the end of lines
    if (strm->here.type == TOK_UNKNOWN_SPACE) {
      assert(strm->next != NULL);
      if ( strm->next->here.type == TOK_UNKNOWN_NEWLINE
        || strm->next->here.type == TOK_EOF
         ) {
        strm->here.transparent = true;
        eexprError err = {.loc = strm->here.loc, .type = EEXPRERR_TRAILING_SPACE};
        dllist_insertAfter_eexprError(&st->errStream, NULL, &err);
      }
      else if (strm->here.as.unknownSpace.chr == escapeLeader) {
        dllistNode_token* prev = getPrev(strm);
        if (prev->here.type == TOK_UNKNOWN_SPACE) {
          prev->here.transparent = true;
        }
        dllistNode_token* next = getNext(strm);
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
  for (dllistNode_token* strm = st->tokStream.start; strm != NULL; strm = strm->next) {
    if (strm->here.transparent) { continue; }
    if (strm->here.type == TOK_UNKNOWN_NEWLINE) {
      dllistNode_token* next = getNext(strm);
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
  `space_1 unknown-dot ^(space | end-of-line)_2 --> \1 predot \2`
  `unknown-dot --> error`
*/
void disambiguateDots(lexer* st) {
  for (dllistNode_token* strm = st->tokStream.start; strm != NULL; strm = strm->next) {
    if (strm->here.transparent) { continue; }
    if (strm->here.type == TOK_UNKNOWN_DOT) {
      dllistNode_token* prev = getPrev(strm);
      bool spaceBefore = prev            == NULL
                      || prev->here.type == TOK_NEWLINE
                      || prev->here.type == TOK_SPACE
                       ;
      bool trueSpaceBefore = prev != NULL && prev->here.type == TOK_SPACE;
      dllistNode_token* next = getNext(strm);
      bool spaceAfter = next->here.type == TOK_EOF
                     || next->here.type == TOK_NEWLINE
                     || next->here.type == TOK_SPACE
                      ;
      if (!spaceBefore && !spaceAfter) {
        strm->here.type = TOK_CHAIN;
      }
      else if (trueSpaceBefore && !spaceAfter) {
        strm->here.type = TOK_PREDOT;
      }
      else {
        eexprError err = {.loc = strm->here.loc, .type = EEXPRERR_BAD_DOT};
        dllist_insertAfter_eexprError(&st->errStream, NULL, &err);
      }
    }
  }
}

/*
  `unknown-colon end-of-line whitespace(n)?_1 --> open-indent(n || 0) end-of-line \1`
  `unknown-colon ^end-of-line --> colon
*/
void disambiguateColons(lexer* st) {
  for (dllistNode_token* strm = st->tokStream.start; strm != NULL; strm = strm->next) {
    if (strm->here.transparent) { continue; }
    if (strm->here.type == TOK_UNKNOWN_COLON) {
      dllistNode_token* next = getNext(strm);
      if (next->here.type == TOK_UNKNOWN_NEWLINE) {
        dllistNode_token* ws = getNext(next);
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
    else if (strm->here.type == TOK_WRAP && strm->here.as.wrap.isOpen) {
      dllistNode_token* newline = getNext(strm);
      if (newline->here.type == TOK_UNKNOWN_NEWLINE) {
        dllistNode_token* ws = getNext(newline);
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

#define TYPE size_t
#include "shim/dynarr.h"
size_t indentState_peek(const dynarr_size_t* st) {
  return st->len == 0 ? 0 : st->data[st->len-1];
}
size_t indentState_pop(dynarr_size_t* st) {
  return st->len == 0 ? 0 : st->data[--st->len];
}

bool insertDedents(lexer* st, dynarr_size_t* depths, dllistNode_token* endOfLine) {
  size_t newDepth;
  dllistNode_token* insertPoint;
  if (endOfLine->here.type == TOK_UNKNOWN_NEWLINE) {
    dllistNode_token* maybeSpace = getNext(endOfLine);
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
      token tok = {.loc = loc, .type = TOK_WRAP, .as.wrap = {.type = WRAP_BLOCK, .isOpen = false}};
      lexer_insertBefore(st, &tok, insertPoint);
      indentState_pop(depths);
    }
    else if (newDepth == depth) {
      if (insertPoint->here.type == TOK_WRAP && !insertPoint->here.as.wrap.isOpen) {
        // do nothing: supress newline between dedent and close wrap
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
      eexprError err = {.loc = loc, .type = EEXPRERR_OFFSIDES};
      dllist_insertAfter_eexprError(&st->errStream, NULL, &err);
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
bool detectIndentation(lexer* st) {
  bool success = false;
  dynarr_size_t depths; dynarr_init_size_t(&depths, 30);
  for (dllistNode_token* strm = st->tokStream.start; strm != NULL; strm = strm->next) {
    if (strm->here.transparent) { continue; }
    if (strm->here.type == TOK_OPEN_INDENT) {
      dllistNode_token* next = getNext(strm);
      fileloc loc =
        { .start = {.line = next->here.loc.start.line, .col = 0}
        , .end = next->here.loc.start
        };
      size_t depth = strm->here.as.indent.depth;
      size_t depth0 = indentState_peek(&depths);
      if (depth > depth0) {
        dynarr_push_size_t(&depths, &depth);
        token tok = {.loc = loc, .type = TOK_WRAP, .as.wrap = {.type = WRAP_BLOCK, .isOpen = true}};
        lexer_insertBefore(st, &tok, next);
        strm->here.transparent = true;
        success = true;
      }
      else {
        eexprError err = {.loc = loc, .type = EEXPRERR_SHALLOW_INDENT};
        dllist_insertAfter_eexprError(&st->errStream, NULL, &err);
        success = false;
      }
    }
    else if ( strm->here.type == TOK_UNKNOWN_NEWLINE
           || strm->here.type == TOK_EOF
            ) {
      success = insertDedents(st, &depths, strm);
    }
  }
  dynarr_deinit_size_t(&depths);
  return success;
}

// This really just checks that newlines and inline space have all been handled.
void disambiguateSpaces(lexer* st) {
  for (dllistNode_token* strm = st->tokStream.start; strm != NULL; strm = strm->next) {
    if (strm->here.transparent) { continue; }
    // all newlines should already have been handled
    assert (strm->here.type != TOK_UNKNOWN_NEWLINE);
    if (strm->here.type == TOK_UNKNOWN_SPACE) {
      // we (should already) know this is inline space
      strm->here.type = TOK_SPACE;
      // we should already have merged adjacent spaces
      {
        dllistNode_token* prev = getPrev(strm);
        assert(prev->here.type != TOK_SPACE);
      }
      // space at start of a line should already have been handled
      {
        dllistNode_token* prev = getPrev(strm);
        assert(prev != NULL);
        assert(prev->here.type != TOK_NEWLINE);
      }
      // space at end of line should already have been handled
      {
        dllistNode_token* next = getPrev(strm);
        assert(next->here.type != TOK_NEWLINE);
        assert(next->here.type != TOK_EOF);
      }
    }
  }
}

/*
  `(chain | ellipsis) (chain | ellipsis) --> error`
  `number chain --> error`
  `(number | symbol) (number | symbol) --> error`
  `string.(close | plain) string.(open | plain) --> error`
*/
void detectCramming(lexer* st) {
  for (dllistNode_token* strm = st->tokStream.start; strm != NULL; strm = strm->next) {
    if (strm->here.transparent) { continue; }
    if (strm->here.type == TOK_EOF) { continue; }
    enum tokenType hereType = strm->here.type;
    bool hereIsDotLike = hereType == TOK_ELLIPSIS || hereType == TOK_CHAIN || hereType == TOK_PREDOT;
    dllistNode_token* next = getNext(strm);
    enum tokenType nextType = next->here.type;
    bool nextIsDotLike = nextType == TOK_ELLIPSIS || nextType == TOK_CHAIN || nextType == TOK_PREDOT;
    fileloc loc = {.start = strm->here.loc.start, .end = next->here.loc.end};
    eexprError err = {.loc = loc, .type = EEXPRERR_CRAMMED_TOKENS};
    if (hereIsDotLike && nextIsDotLike) {
      dllist_insertAfter_eexprError(&st->errStream, NULL, &err);
    }
    else if (hereType == TOK_NUMBER && nextType == TOK_CHAIN) {
      dllist_insertAfter_eexprError(&st->errStream, NULL, &err);
    }
    else if (hereType == TOK_SYMBOL || hereType == TOK_NUMBER) {
      if (nextType == TOK_SYMBOL || nextType == TOK_NUMBER) {
        dllist_insertAfter_eexprError(&st->errStream, NULL, &err);
      }
    }
    else if (hereType == TOK_STRING && nextType == TOK_STRING) {
      bool hereStringClosed = strm->here.as.string.splice == STRSPLICE_PLAIN || strm->here.as.string.splice == STRSPLICE_CLOSE;
      bool nextStringOpen = strm->here.as.string.splice == STRSPLICE_PLAIN || strm->here.as.string.splice == STRSPLICE_OPEN;
      if (hereStringClosed && nextStringOpen) {
        dllist_insertAfter_eexprError(&st->errStream, NULL, &err);
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
  // TODO detect mixed newlines
  // TODO create error if file starts with indent
}
