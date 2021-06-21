#include "eexpr.h"

#include <assert.h>
#include <stdlib.h>

#include "common.h"
#include "engine.h"


struct eexpr_parserInternal {
  engine st;
  struct outputCaps {
    size_t eexprs;
    size_t tokens;
    size_t errors;
    size_t warnings;
  } caps;
  enum eexpr_parsePauseAt resumeFrom;
};


static
void appendError(eexpr_parser* parser, const eexpr_error* err) {
  if (parser->nErrors == parser->impl->caps.errors) {
    parser->impl->caps.errors = parser->impl->caps.errors < 8 ? 8 : 2 * parser->impl->caps.errors;
    eexpr_error* new = realloc(parser->errors, sizeof(eexpr_error) * parser->impl->caps.errors);
    checkOom(new);
    parser->errors = new;
  }
  parser->errors[parser->nErrors] = *err;
  parser->nErrors += 1;
}
static
void appendWarning(eexpr_parser* parser, const eexpr_error* err) {
  if (parser->nWarnings == parser->impl->caps.warnings) {
    parser->impl->caps.warnings = parser->impl->caps.warnings < 8 ? 8 : 2 * parser->impl->caps.warnings;
    eexpr_error* new = realloc(parser->warnings, sizeof(eexpr_error) * parser->impl->caps.warnings);
    checkOom(new);
    parser->warnings = new;
  }
  parser->warnings[parser->nWarnings] = *err;
  parser->nWarnings += 1;
}

static
void drainErrors(eexpr_parser* parser) {
  for (dllistNode_eexpr_error* err = parser->impl->st.errStream.start; err != NULL; err = err->next) {
    bool isError;
    switch (err->here.type) {
      case EEXPR_ERR_MIXED_SPACE: { isError = parser->isError.mixedSpace; } break;
      case EEXPR_ERR_MIXED_NEWLINES: { isError = parser->isError.mixedNewlines; } break;
      case EEXPR_ERR_BAD_DIGIT_SEPARATOR: { isError = parser->isError.badDigitSeparator; } break;
      case EEXPR_ERR_TRAILING_SPACE: { isError = parser->isError.trailingSpace; } break;
      case EEXPR_ERR_NO_TRAILING_NEWLINE: { isError = parser->isError.noTrailingNewline; } break;
      default: { isError = true; } break;
    }
    if (isError) {
      appendError(parser, &err->here);
    }
    else {
      appendWarning(parser, &err->here);
    }
  }
  dllist_del_eexpr_error(&parser->impl->st.errStream);
  if (parser->impl->st.fatal.type != EEXPR_ERR_NOERROR) {
    appendError(parser, &parser->impl->st.fatal);
  }
}

static
void appendToken(eexpr_parser* parser, eexpr_token* tok) {
  if (parser->nTokens == parser->impl->caps.tokens) {
    parser->impl->caps.tokens = parser->impl->caps.tokens < 8 ? 8 : 2 * parser->impl->caps.tokens;
    eexpr_token** new = realloc(parser->tokens, sizeof(eexpr_token*) * parser->impl->caps.tokens);
    checkOom(new);
    parser->tokens = new;
  }
  parser->tokens[parser->nTokens] = tok;
  parser->nTokens += 1;
}
static
void drainTokens(eexpr_parser* parser) {
  if (parser->pauseAt < EEXPR_PAUSE_AFTER_PARSE) {
    parser->nTokens = 0;
    for (dllistNode_eexpr_token* tok = parser->impl->st.tokStream.start; tok != NULL; tok = tok->next) {
      appendToken(parser, &tok->here);
    }
  }
  else {
    if (parser->tokens != NULL) {
      free(parser->tokens);
      parser->tokens = NULL;
    }
    parser->nTokens = 0;
  }

}
static
void drainEexprs(eexpr_parser* parser) {
  parser->nEexprs = parser->impl->st.eexprStream.len;
  parser->eexprs = parser->impl->st.eexprStream.data;
  parser->impl->st.eexprStream.len = 0;
  parser->impl->st.eexprStream.cap = 0;
  parser->impl->st.eexprStream.data = NULL;
}

bool eexpr_parse(eexpr_parser* parser, size_t nBytes, uint8_t* utf8Input) {
  if (parser->impl == NULL) { goto start; }
  else {
    assert(nBytes == 0);
    assert(utf8Input == NULL);
    switch(parser->impl->resumeFrom) {
      case EEXPR_PAUSE_AFTER_START: goto rawlex;
      case EEXPR_PAUSE_AFTER_RAWLEX: goto cooklex;
      case EEXPR_PAUSE_AFTER_COOKLEX: goto parse;
      case EEXPR_PAUSE_AFTER_PARSE: goto finish;
      case EEXPR_DO_NOT_PAUSE: goto finish;
    }
  } assert(false);

  start: {
    // initialize internals
    parser->impl = malloc(sizeof(eexpr_parserInternal));
    checkOom(parser->impl);
    // save input capacities; initialize output lengths
    parser->impl->caps.eexprs = parser->nEexprs; parser->nEexprs = 0;
    parser->impl->caps.tokens = parser->nTokens; parser->nTokens = 0;
    parser->impl->caps.errors = parser->nErrors; parser->nErrors = 0;
    parser->impl->caps.warnings = parser->nWarnings; parser->nWarnings = 0;
    // initialize the engine
    parser->impl->st = engine_newFromStrn(nBytes, utf8Input);
    // save progress and possibly pause
    parser->impl->resumeFrom = EEXPR_PAUSE_AFTER_START;
    if (parser->pauseAt == EEXPR_PAUSE_AFTER_START) { return true; }
  }

  rawlex: {
    if (parser->nErrors != 0) { return false; }
    engine_rawLex(&parser->impl->st);
    drainTokens(parser);
    drainErrors(parser);
    // save progress and possibly pause
    parser->impl->resumeFrom = EEXPR_PAUSE_AFTER_RAWLEX;
    if (parser->pauseAt == EEXPR_PAUSE_AFTER_RAWLEX) { return true; }
  }

  cooklex: {
    if (parser->nErrors != 0) { return false; }
    engine_cookLex(&parser->impl->st);
    drainTokens(parser);
    drainErrors(parser);
    assert(parser->impl->st.tokStream.start != NULL);
    // save progress and possibly pause
    parser->impl->resumeFrom = EEXPR_PAUSE_AFTER_COOKLEX;
    if (parser->pauseAt == EEXPR_PAUSE_AFTER_COOKLEX) { return true; }
  }

  parse: {
    if (parser->nErrors != 0) { return false; }
    if (parser->tokens != NULL) {
      free(parser->tokens);
      parser->nTokens = 0;
      parser->tokens = NULL;
    }
    engine_parse(&parser->impl->st);
    drainEexprs(parser);
    drainErrors(parser);
    // save progress and possibly pause
    parser->impl->resumeFrom = EEXPR_PAUSE_AFTER_PARSE;
    if (parser->pauseAt == EEXPR_PAUSE_AFTER_PARSE) { return true; }
  }

  finish: {
    if (parser->nErrors != 0) { return false; }
    return true;
  }
}



void eexpr_parserInitDefault(eexpr_parser* parser) {
  parser->nEexprs = 0; parser->eexprs = NULL;
  parser->nTokens = 0; parser->tokens = NULL;
  parser->nErrors = 0; parser->errors = NULL;
  parser->nWarnings = 0; parser->warnings = NULL;
  struct eexpr_parseErrorLevels opts = { false, false, false, false, false };
  parser->isError = opts;
  parser->pauseAt = EEXPR_DO_NOT_PAUSE;
  parser->impl = NULL;
}

void eexpr_parser_deinit(eexpr_parser* parser) {
  if (parser->impl == NULL) { return; }
  if (parser->tokens != NULL) { free(parser->tokens); }
  if (parser->eexprs == parser->impl->st.eexprStream.data) {
    // transfer ownership of the output eexprs to the caller
    parser->impl->st.eexprStream.len = 0;
    parser->impl->st.eexprStream.cap = 0;
    parser->impl->st.eexprStream.data = NULL;
  }
  engine_deinit(&parser->impl->st);
  free(parser->impl); // free the internal state
  parser->impl = NULL;
}


//////////////////////////////////// `eexpr_as*` Functions ////////////////////////////////////

void eexpr_del(eexpr* self) {
  if (self == NULL) { return; }
  eexpr_deinit(self);
  free(self);
}

void eexpr_deinit(eexpr* self) {
  if (self == NULL) { return; }
  switch (self->type) {
    case EEXPR_SYMBOL: {
      if (self->as.symbol.text.bytes != NULL) { free(self->as.symbol.text.bytes); }
    }; break;
    case EEXPR_NUMBER: {
      free(self->as.number.mantissa.buf);
      free(self->as.number.exponent.buf);
    }; break;
    case EEXPR_STRING: {
      free(self->as.string.text1.bytes);
      for (size_t i = 0; i < self->as.string.parts.len; ++i) {
        eexpr_del(self->as.string.parts.data[i].subexpr);
        free(self->as.string.parts.data[i].utf8str);
      }
      dynarr_deinit_strTemplPart(&self->as.string.parts);
    }; break;
    case EEXPR_PAREN: {
      if (self->as.wrap != NULL) {
        eexpr_del(self->as.wrap);
      }
    }; break;
    case EEXPR_BRACK: {
      if (self->as.wrap != NULL) {
        eexpr_del(self->as.wrap);
      }
    }; break;
    case EEXPR_BRACE: {
      if (self->as.wrap != NULL) {
        eexpr_del(self->as.wrap);
      }
    }; break;
    case EEXPR_BLOCK: {
      for (size_t i = 0; i < self->as.list.len; ++i) {
        eexpr_del(self->as.list.data[i]);
      }
      dynarr_deinit_eexpr_p(&self->as.list);
    }; break;
    case EEXPR_PREDOT: {
      eexpr_del(self->as.wrap);
    }; break;
    case EEXPR_CHAIN: {
      for (size_t i = 0; i < self->as.list.len; ++i) {
        eexpr_del(self->as.list.data[i]);
      }
      dynarr_deinit_eexpr_p(&self->as.list);
    }; break;
    case EEXPR_SPACE: {
      for (size_t i = 0; i < self->as.list.len; ++i) {
        eexpr_del(self->as.list.data[i]);
      }
      dynarr_deinit_eexpr_p(&self->as.list);
    }; break;
    case EEXPR_ELLIPSIS: {
      if (self->as.ellipsis[0] != NULL) {
        eexpr_del(self->as.ellipsis[0]);
      }
      if (self->as.ellipsis[1] != NULL) {
        eexpr_del(self->as.ellipsis[1]);
      }
    }; break;
    case EEXPR_COLON: {
      eexpr_del(self->as.pair[0]);
      eexpr_del(self->as.pair[1]);
    }; break;
    case EEXPR_COMMA: {
      for (size_t i = 0; i < self->as.list.len; ++i) {
        eexpr_del(self->as.list.data[i]);
      }
      dynarr_deinit_eexpr_p(&self->as.list);
    }; break;
    case EEXPR_SEMICOLON: {
      for (size_t i = 0; i < self->as.list.len; ++i) {
        eexpr_del(self->as.list.data[i]);
      }
      dynarr_deinit_eexpr_p(&self->as.list);
    }; break;
  }
}


eexpr_loc eexpr_locate(const eexpr* self) {
  return self->loc;
}

eexpr_type eexpr_getType(const eexpr* self) {
  return self->type;
}


bool eexpr_asSymbol(const eexpr* self, size_t* nBytes, uint8_t** utf8str) {
  if (self->type != EEXPR_SYMBOL) { return false; }
  *nBytes = self->as.symbol.text.len;
  * utf8str = self->as.symbol.text.bytes;
  return true;
}

bool eexpr_asNumber(const eexpr* self, eexpr_number* value) {
  if (self->type != EEXPR_NUMBER) { return false; }
  value->isPositive = self->as.number.mantissa.pos;
  value->nBigDigits = self->as.number.mantissa.len;
  value->bigDigits = self->as.number.mantissa.buf;
  value->radix = self->as.number.radix;
  value->nFracDigits = self->as.number.fractionalDigits;
  value->isPositive_exp = self->as.number.exponent.pos;
  value->nBigDigits_exp = self->as.number.exponent.len;
  value->bigDigits_exp = self->as.number.exponent.buf;
  assert(value->nBigDigits == 0 ? (value->bigDigits == NULL && !value->isPositive) : true);
  assert(value->nBigDigits_exp == 0 ? (value->bigDigits_exp == NULL && !value->isPositive_exp) : true);
  return true;
}

bool eexpr_asString(const eexpr* self, eexpr_string* value) {
  if (self->type != EEXPR_STRING) { return false; }
  if (value != NULL) {
    value->head.nBytes = self->as.string.text1.len;
    value->head.utf8str = self->as.string.text1.bytes;
    value->nSubexprs = self->as.string.parts.len;
    value->tail = self->as.string.parts.data;
  }
  return true;
}

bool eexpr_asParen(const eexpr* self, eexpr** subexpr) {
  if (self->type != EEXPR_PAREN) { return false; }
  if (subexpr != NULL) { *subexpr = self->as.wrap; }
  return true;
}

bool eexpr_asBrack(const eexpr* self, eexpr** subexpr) {
  if (self->type != EEXPR_BRACK) { return false; }
  if (subexpr != NULL) { *subexpr = self->as.wrap; }
  return true;
}

bool eexpr_asBrace(const eexpr* self, eexpr** subexpr) {
  if (self->type != EEXPR_BRACE) { return false; }
  if (subexpr != NULL) { *subexpr = self->as.wrap; }
  return true;
}

bool eexpr_asBlock(const eexpr* self, size_t* nSubexprs, eexpr*** subexprs) {
  if (self->type != EEXPR_BLOCK) { return false; }
  if (nSubexprs != NULL) { *nSubexprs = self->as.list.len; }
  if (subexprs != NULL) { *subexprs = self->as.list.data; }
  return true;
}

bool eexpr_asPredot(const eexpr* self, eexpr** subexpr) {
  if (self->type != EEXPR_PREDOT) { return false; }
  if (subexpr != NULL) { *subexpr = self->as.wrap; }
  return true;
}

bool eexpr_asChain(const eexpr* self, size_t* nSubexprs, eexpr*** subexprs) {
  if (self->type != EEXPR_CHAIN) { return false; }
  if (nSubexprs != NULL) { *nSubexprs = self->as.list.len; }
  if (subexprs != NULL) { *subexprs = self->as.list.data; }
  return true;
}

bool eexpr_asSpace(const eexpr* self, size_t* nSubexprs, eexpr*** subexprs) {
  if (self->type != EEXPR_SPACE) { return false; }
  if (nSubexprs != NULL) { *nSubexprs = self->as.list.len; }
  if (subexprs != NULL) { *subexprs = self->as.list.data; }
  return true;
}

bool eexpr_asEllipsis(const eexpr* self, eexpr** before, eexpr** after) {
  if (self->type != EEXPR_ELLIPSIS) { return false; }
  if (before != NULL) { *before = self->as.ellipsis[0]; }
  if (after != NULL) { *after = self->as.ellipsis[1]; }
  return true;
}

bool eexpr_asColon(const eexpr* self, eexpr** before, eexpr** after) {
  if (self->type != EEXPR_COLON) { return false; }
  assert(self->as.ellipsis[0] != NULL);
  assert(self->as.ellipsis[1] != NULL);
  if (before != NULL) { *before = self->as.ellipsis[0]; }
  if (after != NULL) { *after = self->as.ellipsis[1]; }
  return true;
}

bool eexpr_asComma(const eexpr* self, size_t* nSubexprs, eexpr*** subexprs) {
  if (self->type != EEXPR_COMMA) { return false; }
  if (nSubexprs != NULL) { *nSubexprs = self->as.list.len; }
  if (subexprs != NULL) { *subexprs = self->as.list.data; }
  return true;
}

bool eexpr_asSemicolon(const eexpr* self, size_t* nSubexprs, eexpr*** subexprs) {
  if (self->type != EEXPR_SEMICOLON) { return false; }
  if (nSubexprs != NULL) { *nSubexprs = self->as.list.len; }
  if (subexprs != NULL) { *subexprs = self->as.list.data; }
  return true;
}


//////////////////////////////////// `eexpr_tokenAs*` Functions ////////////////////////////////////

eexpr_tokenType eexpr_getTokenType(const eexpr_token* self) {
  return self->type;
}

bool eexpr_tokenIsTransparent(const eexpr_token* self) {
  return self->transparent;
}

eexpr_loc eexpr_tokenLocate(const eexpr_token* self) {
  return self->loc;
}


bool eexpr_tokenAsSymbol(const eexpr_token* self, size_t* nBytes, uint8_t** utf8str) {
  if (self->type != EEXPR_TOK_SYMBOL) { return false; }
  if (nBytes != NULL) { *nBytes = self->as.string.text.len; }
  if (utf8str != NULL) { *utf8str = self->as.string.text.bytes; }
  return true;
}

bool eexpr_tokenAsNumber(const eexpr_token* self, eexpr_number* value) {
  if (self->type != EEXPR_TOK_NUMBER) { return false; }
  if (value != NULL) {
    assert(self->as.number.mantissa.len == 0
          ? (self->as.number.mantissa.buf == NULL && !self->as.number.mantissa.pos)
          : true);
    assert(self->as.number.exponent.len == 0
          ? (self->as.number.exponent.buf == NULL && !self->as.number.exponent.pos)
          : true);
    value->isPositive = self->as.number.mantissa.pos;
    value->nBigDigits = self->as.number.mantissa.len;
    value->bigDigits = self->as.number.mantissa.buf;
    value->radix = self->as.number.radix;
    value->nFracDigits = self->as.number.fractionalDigits;
    value->isPositive_exp = self->as.number.exponent.pos;
    value->nBigDigits_exp = self->as.number.exponent.len;
    value->bigDigits_exp = self->as.number.exponent.buf;
  }
  return true;
}

bool eexpr_tokenAsString(const eexpr_token* self, eexpr_stringType* type, size_t* nBytes, uint8_t** utf8str) {
  if (self->type != EEXPR_TOK_STRING) { return false; }
  if (type != NULL) { *type = self->as.string.splice; }
  if (nBytes != NULL) { *nBytes = self->as.string.text.len; }
  if (utf8str != NULL) { *utf8str = self->as.string.text.bytes; }
  return true;
}

bool eexpr_tokenAsSpace(const eexpr_token* self, eexpr_spaceType* type, size_t* nChars) {
  if ( self->type != EEXPR_TOK_SPACE
    && self->type != EEXPR_TOK_UNKNOWN_SPACE
     ) { return false; }
  if (type != NULL) { *type = self->as.unknownSpace.type; }
  if (nChars != NULL) { *nChars = self->as.unknownSpace.size; }
  return true;
}

bool eexpr_tokenAsIndent(const eexpr_token* self, size_t* depth) {
  if (self->type != EEXPR_TOK_INDENT) { return false; }
  if (depth != NULL) { *depth = self->as.indent.depth; }
  return true;
}

bool eexpr_tokenAsWrap(const eexpr_token* self, eexpr_wrapType* type, bool* isOpen) {
  if (self->type != EEXPR_TOK_WRAP) { return false; }
  if (type != NULL) { *type = self->as.wrap.type; }
  if (isOpen != NULL) { *isOpen = self->as.wrap.isOpen; }
  return true;
}
