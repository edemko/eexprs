#include <assert.h>

#include "parser/util.h"
#include "shim/common.h"


//////////////////////////////////// Helper Procedures ////////////////////////////////////

static
void mkUnbalanceError(engine* st) {
  if (st->fatal.type != EEXPRERR_NOERROR) { return; }
  eexpr_token* lookahead = parser_peek(st);
  st->fatal.type = EEXPRERR_UNBALANCED_WRAP;
  st->fatal.loc = lookahead->loc;
  if (st->wrapStack.len == 0) {
    st->fatal.as.unbalancedWrap.type = WRAP_NULL;
  }
  else {
    const openWrap* match = dynarr_peek_openWrap(&st->wrapStack);
    st->fatal.as.unbalancedWrap.type = match->type;
    st->fatal.as.unbalancedWrap.loc = match->loc;
  }
}


//////////////////////////////////// Individual Expression Parsers ////////////////////////////////////

static eexpr* parseSemicolon(engine* st);
static eexpr* parseSpace(engine* st);

/*
```
wrapExpr
  ::= '(' semicolonExpr? ')'
   |  '[' semicolonExpr? ']'
   |  '{' semicolonExpr? '}'
   |  indent semicolonExpr (newline semicolonExpr)* dedent
```
*/
static
eexpr* parseWrap(engine* st) {
  eexpr_token* open = parser_peek(st);
  if ( open->type != EEXPR_TOK_WRAP
    || !open->as.wrap.isOpen
     ) { return NULL; }
  eexpr* out = malloc(sizeof(eexpr));
  checkOom(out);
  {
    openWrap openInfo = {.loc = open->loc, .type = open->as.wrap.type};
    switch (open->as.wrap.type) {
      case WRAP_NULL: assert(false);
      case WRAP_PAREN: {
        dynarr_push_openWrap(&st->wrapStack, &openInfo);
        out->type = EEXPR_PAREN;
        goto nonIndent;
      }; break;
      case WRAP_BRACK: {
        dynarr_push_openWrap(&st->wrapStack, &openInfo);
        out->type = EEXPR_BRACK;
        goto nonIndent;
      }; break;
      case WRAP_BRACE: {
        dynarr_push_openWrap(&st->wrapStack, &openInfo);
        out->type = EEXPR_BRACE;
        goto nonIndent;
      }; break;
      case WRAP_BLOCK: {
        dynarr_push_openWrap(&st->wrapStack, &openInfo);
        out->type = EEXPR_BLOCK;
        goto indent;
      }; break;
    }
  } assert(false);
  nonIndent: {
    out->loc.start = open->loc.start;
    parser_pop(st);
    out->as.wrap = parseSemicolon(st);
    eexpr_token* close = parser_peek(st);
    if ( st->wrapStack.len != 0
      && close->type == EEXPR_TOK_WRAP
      && !close->as.wrap.isOpen
      && close->as.wrap.type == dynarr_peek_openWrap(&st->wrapStack)->type
       ) {
      dynarr_pop_openWrap(&st->wrapStack);
      out->loc.end = close->loc.end;
      parser_pop(st);
    }
    else {
      mkUnbalanceError(st);
    }
    return out;
  } assert(false);
  indent: {
    out->loc.start = open->loc.start;
    parser_pop(st);
    dynarr_init_eexpr_p(&out->as.list, 4);
    while (true) {
      eexpr* subexpr = parseSemicolon(st);
      if (subexpr != NULL) {
        dynarr_push_eexpr_p(&out->as.list, &subexpr);
      }
      eexpr_token* lookahead = parser_peek(st);
      if (lookahead->type == EEXPR_TOK_WRAP) {
        if ( st->wrapStack.len != 0
          && !lookahead->as.wrap.isOpen
          && lookahead->as.wrap.type == dynarr_peek_openWrap(&st->wrapStack)->type
           ) {
          dynarr_pop_openWrap(&st->wrapStack);
          out->loc.end = lookahead->loc.end;
          parser_pop(st);
        }
        else {
          mkUnbalanceError(st);
        }
        return out;
      }
      else if (lookahead->type == EEXPR_TOK_NEWLINE) {
        parser_pop(st);
      }
      else {
        eexpr_error err = {.loc = lookahead->loc, .type = EEXPRERR_EXPECTING_NEWLINE_OR_DEDENT};
        dllist_insertAfter_eexpr_error(&st->errStream, NULL, &err);
        return out;
      }
    }
  }; assert(false);
}

/*
```
stringTemplate
  ::= string.plain
   |  string.open spaceExpr (string.middle spaceExpr)* string.close
```
*/
static
eexpr* parseTemplate(engine* st) {
  eexpr_token* tok = parser_peek(st);
  if (tok->type != EEXPR_TOK_STRING) { return NULL; }
  switch (tok->as.string.splice) {
    case EEXPR_STRPLAIN: {
      eexpr* out = malloc(sizeof(eexpr));
      checkOom(out);
      out->loc = tok->loc;
      out->type = EEXPR_STRING;
      out->as.string.text1 = tok->as.string.text;
      out->as.string.parts.cap = 0;
      out->as.string.parts.len = 0;
      out->as.string.parts.data = NULL;
      parser_pop(st);
      return out;
    }; break;
    case EEXPR_STROPEN: {
      eexpr* out = malloc(sizeof(eexpr));
      checkOom(out);
      { // initialize output buffer
        out->loc = tok->loc;
        out->type = EEXPR_STRING;
        out->as.string.text1 = tok->as.string.text;
        dynarr_init_strTemplPart(&out->as.string.parts, 2);
      }
      { // push to wrapStack
        openWrap info = {.loc = tok->loc, .type = '\"'};
        dynarr_push_openWrap(&st->wrapStack, &info);
      }
      parser_pop(st);
      while (true) {
        strTemplPart part;
        { // we need an expr before the next part of the template
          // but if the next part comes without an expr, we can do some error recovery later
          // we flag that recovery is needed by setting part.expr to NULL
          eexpr_token* lookahead = parser_peek(st);
          if ( lookahead->type != EEXPR_TOK_STRING
            || (lookahead->as.string.splice != EEXPR_STRMIDDLE && lookahead->as.string.splice != EEXPR_STRCLOSE)
             ) {
            part.subexpr = parseSpace(st);
          }
          else {
            part.subexpr = NULL;
          }
        }
        eexpr_token* lookahead = parser_peek(st);
        if (part.subexpr != NULL) {
          out->loc.end = part.subexpr->loc.end;
        }
        else {
          eexpr_error err = {.loc = lookahead->loc, .type = EEXPRERR_MISSING_TEMPLATE_EXPR};
          if ( lookahead->type == EEXPR_TOK_STRING
            && (lookahead->as.string.splice == EEXPR_STRMIDDLE || lookahead->as.string.splice == EEXPR_STRCLOSE)
             ) {
            dllist_insertAfter_eexpr_error(&st->errStream, NULL, &err);
          }
          else {
            st->fatal = err;
            return out;
          }
        }
        if (lookahead->type == EEXPR_TOK_STRING) {
          { // append last template part
            part.nBytes = lookahead->as.string.text.len;
            part.utf8str = lookahead->as.string.text.bytes;
            if (part.subexpr != NULL) {
              dynarr_push_strTemplPart(&out->as.string.parts, &part);
            }
            out->loc.end = lookahead->loc.end;
          }
          // ensure we are expecting a close string
          if (st->wrapStack.len == 0 || dynarr_peek_openWrap(&st->wrapStack)->type != '\"') {
            mkUnbalanceError(st);
          }
          else { // ensure the splice type makes sense
            if (lookahead->as.string.splice == EEXPR_STRCLOSE) {
              dynarr_pop_openWrap(&st->wrapStack);
              parser_pop(st);
              return out;
            }
            else if (lookahead->as.string.splice == EEXPR_STRMIDDLE) {
              parser_pop(st);
            }
            else { assert(false); }
          }
        }
        else {
          if (part.subexpr != NULL) {
            part.nBytes = 0; part.utf8str = NULL;
            dynarr_push_strTemplPart(&out->as.string.parts, &part);
          }
          eexpr_error err = {.loc = lookahead->loc, .type = EEXPRERR_MISSING_CLOSE_TEMPLATE};
          dllist_insertAfter_eexpr_error(&st->errStream, NULL, &err);
          return out;
        }
      }
      assert(false);
    }; break;
    default: {
      mkUnbalanceError(st);
      return NULL;
    }; break;
  }
}

/*
```
atomicExpr
  ::= symbol
   |  number
   |  codepoint
   |  stringTemplate
   |  wrapExpr
```
*/
static
eexpr* parseAtomic(engine* st) {
  eexpr_token* tok = parser_peek(st);
  switch (tok->type) {
    case EEXPR_TOK_SYMBOL: {
      eexpr* out = malloc(sizeof(eexpr));
      checkOom(out);
      out->loc = tok->loc;
      out->type = EEXPR_SYMBOL;
      out->as.symbol = tok->as.symbol;
      parser_pop(st);
      return out;
    }; break;
    case EEXPR_TOK_NUMBER: {
      eexpr* out = malloc(sizeof(eexpr));
      checkOom(out);
      out->loc = tok->loc;
      out->type = EEXPR_NUMBER;
      out->as.number = tok->as.number;
      parser_pop(st);
      return out;
    }; break;
    case EEXPR_TOK_STRING: {
      return parseTemplate(st);
    }; break;
    case EEXPR_TOK_WRAP: {
      return parseWrap(st);
    }; break;
    default: {
      return NULL;
    }
  }
}

/*
```
chainExpr ::= atomicExpr chainTail*
chainTail
  ::= chainDot atomicExpr
   |  wrapExpr
   |  stringTemplate
```
Note that `stringTemplate strTemplPart` is not a chainExpr, since the postlexer should already have detected it as crammed tokens.
*/
static
eexpr* parseChain(engine* st) {
  eexpr* predot = NULL;
  { // check if this is a predot expression
    eexpr_token* lookahead = parser_peek(st);
    if (lookahead->type == EEXPR_TOK_PREDOT) {
      predot = malloc(sizeof(eexpr));
      checkOom(predot);
      predot->type = EEXPR_PREDOT;
      predot->loc.start = lookahead->loc.start;
      parser_pop(st);
    }
  }
  eexpr* chain = NULL;
  {
    { // get the first expression and look for a following dot
      eexpr* expr1 = parseAtomic(st);
      if (expr1 == NULL) {
        goto finish;
      }
      eexpr_token* lookahead = parser_peek(st);
      if ( lookahead->type == EEXPR_TOK_CHAIN
        || (lookahead->type == EEXPR_TOK_WRAP && lookahead->as.wrap.isOpen)
        || ( lookahead->type == EEXPR_TOK_STRING
          && (lookahead->as.string.splice == EEXPR_STRPLAIN || lookahead->as.string.splice == EEXPR_STROPEN)
           )
         ) {
        chain = malloc(sizeof(eexpr));
        checkOom(chain);
        chain->type = EEXPR_CHAIN;
        chain->loc = expr1->loc;
        if (lookahead->type == EEXPR_TOK_CHAIN) {
          chain->loc.end = lookahead->loc.end;
          parser_pop(st);
        }
        dynarr_init_eexpr_p(&chain->as.list, 4);
        dynarr_push_eexpr_p(&chain->as.list, &expr1);
      }
      else {
        chain = expr1;
        goto finish;
      }
    }
    while (true) { // get further chained expressions
      eexpr* next = parseAtomic(st);
      if (next == NULL) { goto finish; }
      dynarr_push_eexpr_p(&chain->as.list, &next);
      eexpr_token* lookahead = parser_peek(st);
      if (lookahead->type == EEXPR_TOK_CHAIN) {
        // continue the chain when there's another chain dot
        chain->loc.end = lookahead->loc.end;
        parser_pop(st);
      }
      else if (lookahead->type == EEXPR_TOK_WRAP && lookahead->as.wrap.isOpen) {
        // continue the chain when there's an open paren/brace/brack/indent
        chain->loc.end = next->loc.end;
      }
      else if ( lookahead->type == EEXPR_TOK_STRING
             && (lookahead->as.string.splice == EEXPR_STRPLAIN || lookahead->as.string.splice == EEXPR_STROPEN)
              ) {
        // continue the chain when there's the start of a string
        chain->loc.end = next->loc.end;
      }
      else {
        chain->loc.end = next->loc.end;
        goto finish;
      }
    }
  } assert(false);
  finish: {
    if (predot == NULL && chain == NULL) {
      return NULL;
    }
    else if (predot == NULL) {
      return chain;
    }
    else {
      predot->loc.end = chain->loc.end;
      predot->as.wrap = chain;
      return predot;
    }
  } assert(false);
}

/*
spaceExpr ::= chainExpr (whitespace chainExpr)*
*/
static
eexpr* parseSpace(engine* st) {
  if (parser_peek(st)->type == EEXPR_TOK_SPACE) {
    parser_pop(st);
  }
  eexpr* expr1 = parseChain(st);
  if (expr1 == NULL) { return NULL; }
  eexpr* out = malloc(sizeof(eexpr));
  { // prepare the output
    checkOom(out);
    out->loc.start = expr1->loc.start;
    out->loc.end = expr1->loc.end;
    out->type = EEXPR_SPACE;
    dynarr_init_eexpr_p(&out->as.list, 4);
    dynarr_push_eexpr_p(&out->as.list, &expr1);
  }
  while (true) {
    eexpr_token* lookahead = parser_peek(st);
    if (lookahead->type == EEXPR_TOK_SPACE) {
      parser_pop(st);
      eexpr* next = parseChain(st);
      if (next != NULL) {
        dynarr_push_eexpr_p(&out->as.list, &next);
        out->loc.end = next->loc.end;
      }
      else {
        goto output;
      }
    }
    else{
      goto output;
    }
  } assert(false);
  output: {
    if (out->as.list.len == 1) {
      dynarr_deinit_eexpr_p(&out->as.list);
      free(out);
      return expr1;
    }
    else {
      // TODO shrink the list? and all other lists generated by the parser?
      return out;
    }
  } assert(false);
}

static
eexpr* parseEllipsis(engine* st) {
  eexpr* expr1 = parseSpace(st);
  eexpr_token* lookahead = parser_peek(st);
  if (lookahead->type == EEXPR_TOK_ELLIPSIS) {
    eexpr_loc dotsLoc = lookahead->loc;
    parser_pop(st);
    eexpr* expr2 = parseSpace(st);
    eexpr* out = malloc(sizeof(eexpr));
    checkOom(out);
    out->type = EEXPR_ELLIPSIS;
    out->loc.start = (expr1 == NULL ? dotsLoc : expr1->loc).start;
    out->loc.end = (expr2 == NULL ? dotsLoc : expr2->loc).end;
    out->as.ellipsis[0] = expr1;
    out->as.ellipsis[1] = expr2;
    return out;
  }
  else {
    return expr1;
  }
}

static
eexpr* parseColon(engine* st) {
  eexpr* expr1 = parseEllipsis(st);
  eexpr_token* colon = parser_peek(st);
  if (colon->type != EEXPR_TOK_COLON) {
    return expr1;
  }
  else {
    eexpr_loc colonLoc = colon->loc;
    parser_pop(st);
    eexpr* expr2 = parseEllipsis(st);
    if (expr2 == NULL) {
      expr1->loc.end = colonLoc.end;
      return expr1;
    }
    eexpr* out = malloc(sizeof(eexpr));
    checkOom(out);
    out->type = EEXPR_COLON;
    out->loc.start = expr1->loc.start;
    out->loc.end = expr2->loc.end;
    out->as.pair[0] = expr1;
    out->as.pair[1] = expr2;
    return out;
  }
}

static
eexpr* parseComma(engine* st) {
  eexpr* out = NULL;
  { // optional initial comma
    eexpr_token* maybeComma = parser_peek(st);
    if (maybeComma->type == EEXPR_TOK_COMMA) {
      out = malloc(sizeof(eexpr));
      checkOom(out);
      dynarr_init_eexpr_p(&out->as.list, 4);
      out->loc = maybeComma->loc;
      parser_pop(st);
    }
  }
  while (true) {
    eexpr* tmp = parseColon(st);
    eexpr_token* lookahead = parser_peek(st);
    if (tmp == NULL) { // no further sub-expressions
      if (out != NULL) {
        out->type = EEXPR_COMMA;
        return out;
      }
      else {
        return NULL;
      }
    }
    else if (out != NULL) { // found a sub-expression, and we already have evidence of a comma
      dynarr_push_eexpr_p(&out->as.list, &tmp);
      if (lookahead->type == EEXPR_TOK_COMMA) { // there's also comma afterwards to be consumed
        out->loc.end = lookahead->loc.end;
        parser_pop(st);
      }
      else {
        out->loc.end = tmp->loc.end;
      }
    }
    else if (lookahead->type == EEXPR_TOK_COMMA) { // found a sub-expression, and the first evidence of a comma
      out = malloc(sizeof(eexpr));
      checkOom(out);
      dynarr_init_eexpr_p(&out->as.list, 4);
      dynarr_push_eexpr_p(&out->as.list, &tmp);
      out->loc.start = tmp->loc.start;
      out->loc.end = lookahead->loc.end;
      parser_pop(st);
    }
    else { // found a sub-expression, with no evidence of a comma before, and no evidence of a comma after
      return tmp;
    }
  }
}

static
eexpr* parseSemicolon(engine* st) {
  eexpr* out = NULL;
  { // optional initial semicolon
    eexpr_token* maybeSemi = parser_peek(st);
    if (maybeSemi->type == EEXPR_TOK_SEMICOLON) {
      out = malloc(sizeof(eexpr));
      checkOom(out);
      dynarr_init_eexpr_p(&out->as.list, 4);
      out->loc = maybeSemi->loc;
      parser_pop(st);
    }
  }
  while (true) {
    eexpr* tmp = parseComma(st);
    eexpr_token* lookahead = parser_peek(st);
    if (tmp == NULL) { // no further sub-expressions
      if (out != NULL) {
        out->type = EEXPR_SEMICOLON;
        return out;
      }
      else {
        return NULL;
      }
    }
    else if (out != NULL) { // found a sub-expression, and we already have evidence of a semicolon
      dynarr_push_eexpr_p(&out->as.list, &tmp);
      if (lookahead->type == EEXPR_TOK_SEMICOLON) { // there's also semicolon afterwards to be consumed
        out->loc.end = lookahead->loc.end;
        parser_pop(st);
      }
      else {
        out->loc.end = tmp->loc.end;
      }
    }
    else if (lookahead->type == EEXPR_TOK_SEMICOLON) { // found a sub-expression, and the first evidence of a semicolon
      out = malloc(sizeof(eexpr));
      checkOom(out);
      dynarr_init_eexpr_p(&out->as.list, 4);
      dynarr_push_eexpr_p(&out->as.list, &tmp);
      out->loc.start = tmp->loc.start;
      out->loc.end = lookahead->loc.end;
      parser_pop(st);
    }
    else { // found a sub-expression, with no evidence of a semicolon before, and no evidence of a semicolon after
      return tmp;
    }
  }
}

//////////////////////////////////// Main Parser ////////////////////////////////////

static
void parseLine(engine* st) {
  eexpr* line = parseSemicolon(st);
  if (line != NULL) {
      dynarr_push_eexpr_p(&st->eexprStream, &line);
  }
  else {
    size_t depth = 0; { // count up how many dedents we currently expect, then reset the wrapStack
      for (size_t i = 0; i < st->wrapStack.len; ++i) {
        if (st->wrapStack.data[i].type == '\n') {
          depth += 1;
        }
      }
      st->wrapStack.len = 0;
    }
    // error recovery: advance through tokens until we get to the next top-level newline (or end-of-file)
    // this should work well because indents and dedents are generated already matched with each other in the postlexer
    while (true) {
      while (depth != 0) {
        eexpr_token* tok = parser_peek(st);
        switch (tok->type) {
          case EEXPR_TOK_EOF: return;
          case EEXPR_TOK_WRAP: {
            if (tok->as.wrap.type == WRAP_BLOCK) {
              if (tok->as.wrap.isOpen) { depth += 1; }
              else { depth -= 1; }
            }
          }; break;
          default: /* do nothing */ break;
        }
        parser_pop(st);
      }
      while (true) {
        eexpr_token* tok = parser_peek(st);
        // consume tokens until next newline/end-of-file,
        if (tok->type == EEXPR_TOK_NEWLINE || tok->type == EEXPR_TOK_EOF) {
          return;
        }
        // but if there's an indent, we'll need to go through matching depths again
        else if ( tok->type == EEXPR_TOK_WRAP
               && tok->as.wrap.type == WRAP_BLOCK
               && tok->as.wrap.isOpen
                ) {
          depth += 1;
          parser_pop(st);
          break;
        }
        else {
          parser_pop(st);
        }
      }
    }
  }
}

void engine_parse(engine* st) {
  bool atStart = true;
  while (st->fatal.type == EEXPRERR_NOERROR) {
    eexpr_token* lookahead = parser_peek(st);
    switch (lookahead->type) {
      case EEXPR_TOK_NEWLINE: {
        assert(!atStart);
        parser_pop(st);
        parseLine(st);
      }; break;
      case EEXPR_TOK_EOF: return;
      default: {
        if (atStart) {
          atStart = false;
          parseLine(st);
        }
        else if ( lookahead->type == EEXPR_TOK_WRAP
               && !lookahead->as.wrap.isOpen
                ) {
          mkUnbalanceError(st);
        }
        else {
          eexpr_token* tok = parser_peek(st);
          fprintf(stderr, "%zu:%zu--%zu:%zu\n", tok->loc.start.line+1, tok->loc.start.col+1, tok->loc.end.line+1, tok->loc.end.col+1);
          assert(atStart);
        }
      } break;
    }
  }
}
