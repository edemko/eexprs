#include "types.h"

#include <stdlib.h>


void token_deinit(eexpr_token* tok) {
  if (tok == NULL) { return; }
  switch (tok->type) {
    case EEXPR_TOK_STRING: {
      if (tok->as.string.text.bytes != NULL) { free(tok->as.string.text.bytes); }
    }; break;
    case EEXPR_TOK_SYMBOL: {
      if (tok->as.symbol.text.bytes != NULL) { free(tok->as.symbol.text.bytes); }
    }; break;
    case EEXPR_TOK_NUMBER: {
      if (tok->as.number.mantissa.buf != NULL) { free(tok->as.number.mantissa.buf); }
      if (tok->as.number.exponent.buf != NULL) { free(tok->as.number.exponent.buf); }
    }; break;
    default: /* do nothing */ break;
  }
}
