#include "types.h"

#include <stdlib.h>


void token_deinit(token* tok) {
  if (tok == NULL) { return; }
  switch (tok->type) {
    case TOK_STRING: {
      if (tok->as.string.text.bytes != NULL) { free(tok->as.string.text.bytes); }
    }; break;
    case TOK_SYMBOL: {
      if (tok->as.symbol.text.bytes != NULL) { free(tok->as.symbol.text.bytes); }
    }; break;
    case TOK_NUMBER: {
      if (tok->as.number.mantissa.buf != NULL) { free(tok->as.number.mantissa.buf); }
      if (tok->as.number.exponent.buf != NULL) { free(tok->as.number.exponent.buf); }
    }; break;
    default: /* do nothing */ break;
  }
}

void eexpr_deinit(eexpr* expr) {
  if (expr == NULL) { return; }
  switch (expr->type) {
    case EEXPR_SYMBOL: {
      if (expr->as.symbol.text.bytes != NULL) { free(expr->as.symbol.text.bytes); }
    }; break;
    case EEXPR_NUMBER: {
      free(expr->as.number.mantissa.buf);
      free(expr->as.number.exponent.buf);
    }; break;
    case EEXPR_CODEPOINT: break;
    case EEXPR_STRING: {
      free(expr->as.string.text1.bytes);
      for (size_t i = 0; i < expr->as.string.parts.len; ++i) {
        eexpr_deinit(expr->as.string.parts.data[i].expr);
        free(expr->as.string.parts.data[i].expr);
        free(expr->as.string.parts.data[i].textAfter.bytes);
      }
      dynarr_deinit(strTemplPart)(&expr->as.string.parts);
    }; break;
    case EEXPR_PAREN: {
      if (expr->as.wrap != NULL) {
        eexpr_deinit(expr->as.wrap);
        free(expr->as.wrap);
      }
    }; break;
    case EEXPR_BRACK: {
      if (expr->as.wrap != NULL) {
        eexpr_deinit(expr->as.wrap);
        free(expr->as.wrap);
      }
    }; break;
    case EEXPR_BRACE: {
      if (expr->as.wrap != NULL) {
        eexpr_deinit(expr->as.wrap);
        free(expr->as.wrap);
      }
    }; break;
    case EEXPR_BLOCK: {
      for (size_t i = 0; i < expr->as.list.len; ++i) {
        eexpr_deinit(expr->as.list.data[i]);
        free(expr->as.list.data[i]);
      }
      dynarr_deinit(eexprPtr)(&expr->as.list);
    }; break;
    case EEXPR_PREDOT: {
      eexpr_deinit(expr->as.wrap);
      free(expr->as.wrap);
    }; break;
    case EEXPR_CHAIN: {
      for (size_t i = 0; i < expr->as.list.len; ++i) {
        eexpr_deinit(expr->as.list.data[i]);
        free(expr->as.list.data[i]);
      }
      dynarr_deinit(eexprPtr)(&expr->as.list);
    }; break;
    case EEXPR_SPACE: {
      for (size_t i = 0; i < expr->as.list.len; ++i) {
        eexpr_deinit(expr->as.list.data[i]);
        free(expr->as.list.data[i]);
      }
      dynarr_deinit(eexprPtr)(&expr->as.list);
    }; break;
    case EEXPR_ELLIPSIS: {
      if (expr->as.ellipsis[0] != NULL) {
        eexpr_deinit(expr->as.ellipsis[0]);
        free(expr->as.ellipsis[0]);
      }
      if (expr->as.ellipsis[1] != NULL) {
        eexpr_deinit(expr->as.ellipsis[1]);
        free(expr->as.ellipsis[1]);
      }
    }; break;
    case EEXPR_COLON: {
      eexpr_deinit(expr->as.pair[0]);
      free(expr->as.pair[0]);
      eexpr_deinit(expr->as.pair[1]);
      free(expr->as.pair[1]);
    }; break;
    case EEXPR_COMMA: {
      for (size_t i = 0; i < expr->as.list.len; ++i) {
        eexpr_deinit(expr->as.list.data[i]);
        free(expr->as.list.data[i]);
      }
      dynarr_deinit(eexprPtr)(&expr->as.list);
    }; break;
    case EEXPR_SEMICOLON: {
      for (size_t i = 0; i < expr->as.list.len; ++i) {
        eexpr_deinit(expr->as.list.data[i]);
        free(expr->as.list.data[i]);
      }
      dynarr_deinit(eexprPtr)(&expr->as.list);
    }; break;
  }
}
