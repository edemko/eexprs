#include "eexpr.h"

#include <assert.h>
#include <stdlib.h>

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
void* useRealloc(void* closure, void* orig_p, size_t newNBytes) {
  return realloc(orig_p, newNBytes);
}


static
void copyError(eexpr_error* dst, const error* src) {
  dst->loc = src->loc;
  dst->type = src->type;
  switch (src->type) {
    case EEXPRERR_NOERROR: { assert(false); }; break;
    case EEXPRERR_BAD_BYTES: {}; break;
    case EEXPRERR_BAD_CHAR: {
      assert(src->as.badChar != UCHAR_NULL);
      dst->as.badChar = src->as.badChar;
    }; break;
    case EEXPRERR_MIXED_SPACE: {}; break;
    case EEXPRERR_MIXED_NEWLINES: {}; break;
    case EEXPRERR_BAD_DIGIT_SEPARATOR: {}; break;
    case EEXPRERR_MISSING_EXPONENT: {}; break;
    case EEXPRERR_BAD_EXPONENT_SIGN: {}; break;
    case EEXPRERR_BAD_ESCAPE_CHAR: {
      assert(src->as.badEscapeChar != UCHAR_NULL);
      dst->as.badEscapeChar = src->as.badEscapeChar;
    }; break;
    case EEXPRERR_BAD_ESCAPE_CODE: {
      for (int i = 0; i < 6; ++i) {
        assert(src->as.badEscapeCode[i] != UCHAR_NULL);
        dst->as.badEscapeCode[i] = src->as.badEscapeCode[i];
      }
    }; break;
    case EEXPRERR_UNICODE_OVERFLOW: {
      assert(src->as.unicodeOverflow != UCHAR_NULL);
      dst->as.unicodeOverflow = src->as.unicodeOverflow;
    }; break;
    case EEXPRERR_BAD_STRING_CHAR: {
      assert(src->as.badStringChar != UCHAR_NULL);
      dst->as.badStringChar = src->as.badStringChar;
    }; break;
    case EEXPRERR_MISSING_LINE_PICKUP: {}; break;
    case EEXPRERR_UNCLOSED_STRING: {}; break;
    case EEXPRERR_UNCLOSED_MULTILINE_STRING: {}; break;
    case EEXPRERR_HEREDOC_BAD_OPEN: {}; break;
    case EEXPRERR_HEREDOC_BAD_INDENT_DEFINITION: {}; break;
    case EEXPRERR_HEREDOC_BAD_INDENTATION: {}; break;
    case EEXPRERR_MIXED_INDENTATION: {
      assert(src->as.mixedIndentation.chr != UCHAR_NULL);
      dst->as.mixedIndentation.chr = src->as.mixedIndentation.chr;
      dst->as.mixedIndentation.loc = src->as.mixedIndentation.loc;
    }; break;
    case EEXPRERR_TRAILING_SPACE: {}; break;
    case EEXPRERR_NO_TRAILING_NEWLINE: {}; break;
    case EEXPRERR_SHALLOW_INDENT: {}; break;
    case EEXPRERR_OFFSIDES: {}; break;
    case EEXPRERR_BAD_DOT: {}; break;
    case EEXPRERR_CRAMMED_TOKENS: {}; break;
    case EEXPRERR_UNBALANCED_WRAP: {
      dst->as.unbalancedWrap.type = src->as.unbalancedWrap.type;
      dst->as.unbalancedWrap.loc = src->as.unbalancedWrap.loc;
    }; break;
    case EEXPRERR_EXPECTING_NEWLINE_OR_DEDENT: {}; break;
    case EEXPRERR_MISSING_TEMPLATE_EXPR: {}; break;
    case EEXPRERR_MISSING_CLOSE_TEMPLATE: {}; break;
  }
}
static
bool appendError(eexpr_parser* parser, const error* err) {
  if (parser->nErrors == parser->impl->caps.errors) {
    parser->impl->caps.errors = parser->impl->caps.errors < 8 ? 8 : 2 * parser->impl->caps.errors;
    eexpr_error* new = parser->allocator(parser->allocClosure, parser->errors, sizeof(eexpr_error) * parser->impl->caps.errors);
    if (new == NULL) { return false; }
    parser->errors = new;
  }
  copyError(&parser->errors[parser->nErrors], err);
  parser->nErrors += 1;
  return true;
}
static
bool appendWarning(eexpr_parser* parser, const error* err) {
  if (parser->nWarnings == parser->impl->caps.warnings) {
    parser->impl->caps.warnings = parser->impl->caps.warnings < 8 ? 8 : 2 * parser->impl->caps.warnings;
    eexpr_error* new = parser->allocator(parser->allocClosure, parser->warnings, sizeof(eexpr_error) * parser->impl->caps.warnings);
    if (new == NULL) { return false; }
    parser->warnings = new;
  }
  copyError(&parser->warnings[parser->nWarnings], err);
  parser->nWarnings += 1;
  return true;
}

static
bool drainErrors(eexpr_parser* parser) {
  for (dllistNode_error* err = parser->impl->st.errStream.start; err != NULL; err = err->next) {
    bool isError;
    switch (err->here.type) {
      case EEXPRERR_MIXED_SPACE: { isError = parser->isError.mixedSpace; } break;
      case EEXPRERR_MIXED_NEWLINES: { isError = parser->isError.mixedNewlines; } break;
      case EEXPRERR_BAD_DIGIT_SEPARATOR: { isError = parser->isError.badDigitSeparator; } break;
      case EEXPRERR_TRAILING_SPACE: { isError = parser->isError.trailingSpace; } break;
      case EEXPRERR_NO_TRAILING_NEWLINE: { isError = parser->isError.noTrailingNewline; } break;
      default: { isError = true; } break;
    }
    if (isError) {
      if (!appendError(parser, &err->here)) { return false; }
    }
    else {
      if (!appendWarning(parser, &err->here)) { return false; }
    }
  }
  dllist_del_error(&parser->impl->st.errStream);
  if (parser->impl->st.fatal.type != EEXPRERR_NOERROR) {
    if (!appendError(parser, &parser->impl->st.fatal)) { return false; }
  }
  return true;
}

static
bool appendToken(eexpr_parser* parser, token* tok) {
  if (parser->nTokens == parser->impl->caps.tokens) {
    parser->impl->caps.tokens = parser->impl->caps.tokens < 8 ? 8 : 2 * parser->impl->caps.tokens;
    token** new = parser->allocator(parser->allocClosure, parser->tokens, sizeof(token*) * parser->impl->caps.tokens);
    if (new == NULL) { return false; }
    parser->tokens = new;
  }
  parser->tokens[parser->nTokens] = tok;
  parser->nTokens += 1;
  return true;
}
static
bool drainTokens(eexpr_parser* parser) {
  if (parser->pauseAt >= EEXPR_PAUSE_AFTER_PARSE) {
    if (parser->tokens != NULL) {
      parser->allocator(parser->allocClosure, parser->tokens, 0);
      parser->tokens = NULL;
    }
    parser->nTokens = 0;
    return true;
  }
  else {
    for (dllistNode_token* tok = parser->impl->st.tokStream.start; tok != NULL; tok = tok->next) {
      if (!appendToken(parser, &tok->here)) { return false; }
    }
    return true;
  }

}
static
bool drainEexprs(eexpr_parser* parser) {
  parser->nEexprs = parser->impl->st.eexprStream.len;
  parser->eexprs = parser->impl->st.eexprStream.data;
  return true;
}

static
void drainLineIndex(eexpr_parser* parser) {
  parser->lines.len = parser->impl->st.lineIndex.len;
  parser->lines.offsets = parser->impl->st.lineIndex.offsets;
  { // transfer ownership of these offsets to the caller of eexpr_parser
    parser->impl->st.lineIndex.len = 0;
    parser->impl->st.lineIndex.cap = 0;
    parser->impl->st.lineIndex.offsets = NULL;
  }
}


bool eexpr_parse(eexpr_parser* parser, size_t nBytes, uint8_t* utf8Input) {
  if (parser->impl == NULL) { goto start; }
  else {
    assert(nBytes == 0);
    assert(utf8Input == NULL);
    switch(parser->impl->resumeFrom) {
      case EEXPR_PAUSE_AT_START: goto rawlex;
      case EEXPR_PAUSE_AFTER_RAWLEX: goto cooklex;
      case EEXPR_PAUSE_AFTER_COOKLEX: goto parse;
      case EEXPR_PAUSE_AFTER_PARSE: goto finishOk;
      case EEXPR_DO_NOT_PAUSE: assert(false);
    }
  } assert(false);

  start: {
    // prepare default allocator if needed
    if (parser->allocator == NULL) { parser->allocator = useRealloc; }
    // initialize internals
    parser->impl = parser->allocator(parser->allocClosure, NULL, sizeof(eexpr_parserInternal));
    if (parser->impl == NULL) { goto finishBad; }
    // save input capacities; initialize output lengths
    parser->impl->caps.eexprs = parser->nEexprs; parser->nEexprs = 0;
    parser->impl->caps.tokens = parser->nTokens; parser->nTokens = 0;
    parser->impl->caps.errors = parser->nErrors; parser->nErrors = 0;
    parser->impl->caps.warnings = parser->nWarnings; parser->nWarnings = 0;
    // initialize the engine
    parser->impl->st = engine_newFromStrn(nBytes, utf8Input);
    // save progress and possibly pause
    parser->impl->resumeFrom = EEXPR_PAUSE_AT_START;
    if (parser->pauseAt == EEXPR_PAUSE_AT_START) { return true; }
  }

  rawlex: {
    engine_rawLex(&parser->impl->st);
    drainLineIndex(parser);
    if (!drainTokens(parser)) { goto finishBad; };
    if (!drainErrors(parser)) { goto finishBad; };
    if (parser->nErrors != 0) {
      goto finishOk;
    }
    // save progress and possibly pause
    parser->impl->resumeFrom = EEXPR_PAUSE_AFTER_RAWLEX;
    if (parser->pauseAt == EEXPR_PAUSE_AFTER_RAWLEX) { return true; }
  }

  cooklex: {
    engine_cookLex(&parser->impl->st);
    if (!drainTokens(parser)) { goto finishBad; };
    if (!drainErrors(parser)) { goto finishBad; };
    if (parser->nErrors != 0) {
      goto finishOk;
    }
    assert(parser->impl->st.tokStream.start != NULL);
    // save progress and possibly pause
    parser->impl->resumeFrom = EEXPR_PAUSE_AFTER_COOKLEX;
    if (parser->pauseAt == EEXPR_PAUSE_AFTER_COOKLEX) { return true; }
  }

  parse: {
    if (parser->tokens != NULL) {
      parser->allocator(parser->allocClosure, parser->tokens, 0);
      parser->nTokens = 0;
      parser->tokens = NULL;
    }

    engine_parse(&parser->impl->st);
    if (!drainEexprs(parser)) { goto finishBad; }
    if (!drainErrors(parser)) { goto finishBad; };
    if (parser->nErrors != 0) {
      goto finishOk;
    }
    // save progress and possibly pause
    parser->impl->resumeFrom = EEXPR_PAUSE_AFTER_PARSE;
    if (parser->pauseAt == EEXPR_PAUSE_AFTER_PARSE) { return true; }
  }

  finishOk: {
    eexpr_parser_deinit(parser);
    return true;
  }; assert(false);
  finishBad: {
    eexpr_parser_deinit(parser);
    return false;
  }; assert(false);
}



void eexpr_parserInitDefault(eexpr_parser* parser) {
  parser->nEexprs = 0; parser->eexprs = NULL;
  parser->nTokens = 0; parser->tokens = NULL;
  parser->nErrors = 0; parser->errors = NULL;
  parser->nWarnings = 0; parser->warnings = NULL;
  parser->lines.len = 0; parser->lines.offsets = NULL;
  struct eexpr_parseErrorLevels opts = { false, false, false, false, false };
  parser->isError = opts;
  parser->allocator = NULL;
  parser->allocClosure = NULL;
  parser->impl = NULL;
}

void eexpr_parser_deinit(eexpr_parser* parser) {
  if (parser->impl == NULL) { return; }
  if (parser->tokens != NULL) { parser->allocator(parser->allocClosure, parser->tokens, 0); }
  if (parser->eexprs == parser->impl->st.eexprStream.data) {
    // transfer ownership of the output eexprs to the caller
    parser->impl->st.eexprStream.len = 0;
    parser->impl->st.eexprStream.cap = 0;
    parser->impl->st.eexprStream.data = NULL;
  }
  engine_deinit(&parser->impl->st);
  assert(parser->allocator != NULL);
  parser->impl = parser->allocator(parser->allocClosure, parser->impl, 0); // free the internal state
}
