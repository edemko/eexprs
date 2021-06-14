#include "app/json.h"

#include <assert.h>
#include <inttypes.h>
#include <stdlib.h>

#include "lexer/util.h"
#include "parser/util.h"
#include "shim/bigint.h"


bool needsJsonEscape(uchar c) {
  return c < 0x20  || c == 0x7F // TODO encode other control codepoints
      || c == '\"' || c == '\\'
       ;
}
void fjsonEscapeChar(FILE* fp, uchar c) {
  if (c < 0 || c > 0x10FFFF) { return; }
  if (c == '\"') {
    fprintf(fp, "\\\"");
  }
  else if (c == '\\') {
    fprintf(fp, "\\\\");
  }
  else if (c == '\n') {
    fprintf(fp, "\\n");
  }
  else if (c < 0x20 || c == 0x7F) { // TODO encode other control codepoints
    fprintf(fp, "\\u00%02X", (uint8_t)c);
  }
  else {
    utf8Char enc = encodeUchar(c);
    fwrite(enc.codeunits, 1/*byte per element*/, enc.nbytes/*many elements*/, fp);
  }
}

void fdumpChar(FILE* fp, uchar c) {
  fprintf(fp, "\"");
  fjsonEscapeChar(fp, c);
  fprintf(fp, "\"");
}
void fdumpStr(FILE* fp, str text) {
  fprintf(fp, "\"");
  while (true) {
    uchar c;
    size_t adv = peekUchar(&c, text);
    if (c == UCHAR_NULL) {
      break;
    }
    else if (c < 0 || c > 0x10FFFF) {
      text.len--;
      text.bytes++;
      continue;
    }
    else if (needsJsonEscape(c)) {
      fjsonEscapeChar(fp, c);
      text.len -= adv;
      text.bytes += adv;
    }
    else {
      fwrite(text.bytes, 1/*byte per element*/, adv/*many elements*/, fp);
      text.len -= adv;
      text.bytes += adv;
    }
  }
  fprintf(fp, "\"");
}
void fdumpCStr(FILE* fp, char* s) {
  str text = {.len = strlen(s), .bytes = (uint8_t*)s};
  fdumpStr(fp, text);
}

void fdumpToken(FILE* fp, const token* tok) {
  fprintf(fp, "{\"loc\":{\"from\":{\"line\":%zu,\"col\":%zu},\"to\":{\"line\":%zu,\"col\":%zu}}"
         , tok->loc.start.line + 1
         , tok->loc.start.col + 1
         , tok->loc.end.line + 1
         , tok->loc.end.col + 1
         );
  if (tok->transparent) {
    fprintf(fp, ",\"ignore\":true");
  }
  switch (tok->type) {
    case TOK_NUMBER: {
      {
        str tmp = bigint_toDecimal(tok->as.number.mantissa);
        fprintf(fp, ",\"type\":\"number\",\"%s\":", tok->as.number.fractionalDigits ? "value" : "mantissa");
        fdumpStr(fp, tmp);
        free(tmp.bytes);
      }
      if (tok->as.number.radix != 10) {
        fprintf(fp, ",\"radix\":%d", tok->as.number.radix);
      }
      if (tok->as.number.fractionalDigits != 0 || tok->as.number.exponent.len != 0) {
        fprintf(fp, ",\"exponent\":{");
        bool needsComma = false;
        if (tok->as.number.fractionalDigits != 0) {
          fprintf(fp, "%s\"fractional\":-%"PRIu32, needsComma ? "," : "", tok->as.number.fractionalDigits);
          needsComma = true;
        }
        if (tok->as.number.exponent.len != 0) {
          str tmp = bigint_toDecimal(tok->as.number.exponent);
          fprintf(fp, "%s\"explicit\":", needsComma ? "," : "");
          fdumpStr(fp, tmp);
          free(tmp.bytes);
        }
        fprintf(fp, "}");
      }
    }; break;
    case TOK_CODEPOINT: {
      fprintf(fp, ",\"type\":\"codepoint\",\"value\":");
      fdumpChar(fp, tok->as.codepoint);
    }; break;
    case TOK_STRING: {
      fprintf(fp, ",\"type\":\"string\",\"text\":");
      fdumpStr(fp, tok->as.string.text);
      switch (tok->as.string.splice) {
        case STRSPLICE_PLAIN: break;
        case STRSPLICE_OPEN: {
          fprintf(fp, ",\"splice\":\"open\"");
        }; break;
        case STRSPLICE_MIDDLE: {
          fprintf(fp, ",\"splice\":\"middle\"");
        }; break;
        case STRSPLICE_CLOSE: {
          fprintf(fp, ",\"splice\":\"close\"");
        }; break;
        case STRSPLICE_CORRUPT: {
          fprintf(fp, ",\"splice\":\"corrupt\"");
        }; break;
      }
    }; break;
    case TOK_SYMBOL: {
      fprintf(fp, ",\"type\":\"symbol\",\"text\":");
      fdumpStr(fp, tok->as.symbol.text);
    }; break;
    case TOK_WRAP: {
      const char* family;
      switch (tok->as.wrap.type) {
        case WRAP_PAREN: family = "paren"; break;
        case WRAP_BRACE: family = "brace"; break;
        case WRAP_BRACK: family = "bracket"; break;
        case WRAP_BLOCK: family = "indent"; break;
        case WRAP_NULL: assert(false);
      }
      const char* open = tok->as.wrap.isOpen ? "true" : "false";
      fprintf(fp, ",\"type\":\"wrap\",\"family\":\"%s\",\"open\":%s", family, open);
    }; break;
    case TOK_COLON: {
      fprintf(fp, ",\"type\":\"colon\"");
    }; break;
    case TOK_ELLIPSIS: {
      fprintf(fp, ",\"type\":\"ellipsis\"");
    }; break;
    case TOK_CHAIN: {
      fprintf(fp, ",\"type\":\"chain\"");
    }; break;
    case TOK_PREDOT: {
      fprintf(fp, ",\"type\":\"predot\"");
    }; break;
    case TOK_SEMICOLON: {
      fprintf(fp, ",\"type\":\"semicolon\"");
    }; break;
    case TOK_COMMA: {
      fprintf(fp, ",\"type\":\"comma\"");
    }; break;
    case TOK_NEWLINE: {
      fprintf(fp, ",\"type\":\"newline\"");
    }; break;
    case TOK_SPACE: {
      fprintf(fp, ",\"type\":\"space\"");
    }; break;
    case TOK_EOF: {
      fprintf(fp, ",\"type\":\"end-of-file\"");
    }; break;
    case TOK_COMMENT: {
      fprintf(fp, ",\"type\":\"comment\"");
    }; break;
    case TOK_OPEN_INDENT: {
      fprintf(fp, ",\"type\":\"open-indent\",\"depth\":%zu", tok->as.indent.depth);
    }; break;
    case TOK_UNKNOWN_SPACE: {
      fprintf(fp, ",\"type\":\"unknown-space\",\"codepoint\":%"PRIi32",\"size\":%zu"
                , tok->as.unknownSpace.chr
                , tok->as.unknownSpace.size
                );
    }; break;
    case TOK_UNKNOWN_NEWLINE: {
      fprintf(fp, ",\"type\":\"unknown-newline\"");
    }; break;
    case TOK_UNKNOWN_COLON: {
      fprintf(fp, ",\"type\":\"unknown-colon\"");
    }; break;
    case TOK_UNKNOWN_DOT: {
      fprintf(fp, ",\"type\":\"unknown-dot\"");
    }; break;
    case TOK_NUMBER_ERROR: {
      fprintf(fp, ",\"type\":\"error-number\"");
    }; break;
    case TOK_STRING_ERROR: {
      fprintf(fp, ",\"type\":\"error-string\"");
    }; break;
    case TOK_NONE: { assert(false); }; break;
  }
  fprintf(fp, "}");
}

void fdumpEexpr(FILE* fp, int indent, const eexpr* expr) {
  fprintf(fp, "{ \"loc\":{\"from\":{\"line\":%zu,\"col\":%zu},\"to\":{\"line\":%zu,\"col\":%zu}}"
         , expr->loc.start.line + 1
         , expr->loc.start.col + 1
         , expr->loc.end.line + 1
         , expr->loc.end.col + 1
         );
  switch (expr->type) {
    case EEXPR_SYMBOL: {
      fprintf(fp, "\n%*s, \"type\":\"symbol\",\"text\":", indent, "");
      fdumpStr(fp, expr->as.symbol.text);
    }; break;
    case EEXPR_NUMBER: {
      {
        str tmp = bigint_toDecimal(expr->as.number.mantissa);
        fprintf( fp, "\n%*s, \"type\":\"number\",\"%s\":"
               , indent, ""
               , expr->as.number.fractionalDigits == 0 ? "value" : "mantissa");
        fdumpStr(fp, tmp);
        free(tmp.bytes);
      }
      if (expr->as.number.radix != 10) {
        fprintf(fp, ",\"radix\":%d", expr->as.number.radix);
      }
      if (expr->as.number.fractionalDigits != 0 || expr->as.number.exponent.len != 0) {
        fprintf(fp, ",\"exponent\":{");
        bool needsComma = false;
        if (expr->as.number.fractionalDigits != 0) {
          fprintf(fp, "%s\"fractional\":-%"PRIu32, needsComma ? "," : "", expr->as.number.fractionalDigits);
          needsComma = true;
        }
        if (expr->as.number.exponent.len != 0) {
          str tmp = bigint_toDecimal(expr->as.number.exponent);
          fprintf(fp, "%s\"explicit\":", needsComma ? "," : "");
          fdumpStr(fp, tmp);
          free(tmp.bytes);
        }
        fprintf(fp, "}");
      }
    }; break;
    case EEXPR_CODEPOINT: {
      fprintf(fp, "\n%*s, \"type\":\"codepoint\",\"value\":", indent, "");
      fdumpChar(fp, expr->as.codepoint);
    }; break;
    case EEXPR_STRING: {
      fprintf(fp, "\n%*s, \"type\":\"string\"", indent, "");
      if (expr->as.string.parts.len == 0) {
        fprintf(fp, ",\"text\":");
        fdumpStr(fp, expr->as.string.text1);
      }
      else {
        fprintf(fp, ",\"template\":\n%*s[ ", indent+2, "");
        fdumpStr(fp, expr->as.string.text1);
        for (size_t i = 0; i < expr->as.string.parts.len; ++i) {
          fprintf(fp, "\n%*s, ", indent+2, "");
          fdumpEexpr(fp, indent+4, expr->as.string.parts.data[i].expr);
          fprintf(fp, "\n%*s, ", indent+2, "");
          fdumpStr(fp, expr->as.string.parts.data[i].textAfter);
        }
        fprintf(fp, "\n%*s]", indent+2, "");
      }
    }; break;
    case EEXPR_PAREN: {
      fprintf(fp, "\n%*s, \"type\":\"paren\"", indent, "");
      if (expr->as.wrap == NULL) {
        fprintf(fp, ",\"subexpr\":null");
      }
      else {
        fprintf(fp, ",\"subexpr\":\n%*s  ", indent, "");
        fdumpEexpr(fp, indent+2, expr->as.wrap);
      }
    }; break;
    case EEXPR_BRACK: {
      fprintf(fp, "\n%*s, \"type\":\"bracket\"", indent, "");
      if (expr->as.wrap == NULL) {
        fprintf(fp, ",\"subexpr\":null");
      }
      else {
        fprintf(fp, ",\"subexpr\":\n%*s  ", indent, "");
        fdumpEexpr(fp, indent+2, expr->as.wrap);
      }
    }; break;
    case EEXPR_BRACE: {
      fprintf(fp, "\n%*s, \"type\":\"brace\"", indent, "");
      if (expr->as.wrap == NULL) {
        fprintf(fp, ",\"subexpr\":null");
      }
      else {
        fprintf(fp, ",\"subexpr\":\n%*s  ", indent, "");
        fdumpEexpr(fp, indent+2, expr->as.wrap);
      }
    }; break;
    case EEXPR_BLOCK: {
      fprintf(fp, "\n%*s, \"type\":\"block\",\"subexprs\":", indent, "");
      fdumpEexprArray(fp, indent+2, &expr->as.list);
    }; break;
    case EEXPR_PREDOT: {
      fprintf(fp, "\n%*s, \"type\":\"predot\",\"subexpr\":", indent, "");
      fdumpEexpr(fp, indent+2, expr->as.wrap);
    }; break;
    case EEXPR_CHAIN: {
      fprintf(fp, "\n%*s, \"type\":\"chain\",\"subexprs\":", indent, "");
      fdumpEexprArray(fp, indent+2, &expr->as.list);
    }; break;
    case EEXPR_SPACE: {
      fprintf(fp, "\n%*s, \"type\":\"space\",\"subexprs\":", indent, "");
      fdumpEexprArray(fp, indent+2, &expr->as.list);
    }; break;
    case EEXPR_ELLIPSIS: {
      fprintf(fp, "\n%*s, \"type\":\"ellipsis\"", indent, "");
      fprintf(fp, "\n%*s, \"before\":", indent, "");
      if (expr->as.ellipsis[0] == NULL) {
        fprintf(fp, "null");
      }
      else {
        fprintf(fp, "\n%*s", indent+2, "");
        fdumpEexpr(fp, indent+2, expr->as.ellipsis[0]);
      }
      fprintf(fp, "\n%*s, \"after\":", indent, "");
      if (expr->as.ellipsis[1] == NULL) {
        fprintf(fp, "null");
      }
      else {
        fprintf(fp, "\n%*s", indent+2, "");
        fdumpEexpr(fp, indent+2, expr->as.ellipsis[1]);
      }
    }; break;
    case EEXPR_COLON: {
      fprintf(fp, "\n%*s, \"type\":\"colon\",\"subexprs\":\n%*s[ ", indent, "", indent+2, "");
      fdumpEexpr(fp, indent+4, expr->as.pair[0]);
      fprintf(fp, "\n%*s, ", indent+2, "");
      fdumpEexpr(fp, indent+4, expr->as.pair[1]);
      fprintf(fp, "\n%*s]", indent+2, "");
    }; break;
    case EEXPR_COMMA: {
      fprintf(fp, "\n%*s, \"type\":\"comma\",\"subexprs\":", indent, "");
      fdumpEexprArray(fp, indent+2, &expr->as.list);
    }; break;
    case EEXPR_SEMICOLON: {
      fprintf(fp, "\n%*s, \"type\":\"semicolon\",\"subexprs\":", indent, "");
      fdumpEexprArray(fp, indent+2, &expr->as.list);}; break;
  }
  fprintf(fp, "\n%*s}", indent, "");
}

void fdumpError(FILE* fp, const eexpr_error* err) {
  fprintf(fp, "{\"loc\":{\"from\":{\"line\":%zu,\"col\":%zu},\"to\":{\"line\":%zu,\"col\":%zu}}"
         , err->loc.start.line + 1
         , err->loc.start.col + 1
         , err->loc.end.line + 1
         , err->loc.end.col + 1
         );
  switch (err->type) {
    case EEXPRERR_NOERROR: { assert(false); }; break;
    case EEXPRERR_BAD_BYTES: {
      fprintf(fp, ",\"type\":\"bad-bytes\"");
    }; break;
    case EEXPRERR_BAD_CHAR: {
      fprintf(fp, ",\"type\":\"bad-char\",\"input\":");
      fdumpChar(fp, err->as.badChar);
    }; break;
    case EEXPRERR_MIXED_SPACE: {
      fprintf(fp, ",\"type\":\"mixed-space\"");
    }; break;
    case EEXPRERR_MIXED_NEWLINES: {
      fprintf(fp, ",\"type\":\"mixed-newlines\"");
    }; break;
    case EEXPRERR_BAD_DIGIT_SEPARATOR: {
      fprintf(fp, ",\"type\":\"bad-digit-separator\"");
    }; break;
    case EEXPRERR_MISSING_EXPONENT: {
      fprintf(fp, ",\"type\":\"missing-exponent\"");
    }; break;
    case EEXPRERR_BAD_EXPONENT_SIGN: {
      fprintf(fp, ",\"type\":\"bad-exponent-sign\"");
    }; break;
    case EEXPRERR_BAD_CODEPOINT: {
      fprintf(fp, ",\"type\":\"bad-codepoint\",\"input\":");
      fdumpChar(fp, err->as.badCodepoint);
    }; break;
    case EEXPRERR_BAD_ESCAPE_CHAR: {
      fprintf(fp, ",\"type\":\"bad-escape-char\",\"input\":");
      fdumpChar(fp, err->as.badEscapeChar);
    }; break;
    case EEXPRERR_BAD_ESCAPE_CODE: {
      fprintf(fp, ",\"type\":\"bad-escape-code\",\"input\":\"");
      for (size_t i = 0; i < 6; ++i) {
        fjsonEscapeChar(fp, err->as.badEscapeCode[i]);
      }
      fprintf(fp, "\"");
    }; break;
    case EEXPRERR_UNICODE_OVERFLOW: {
      fprintf(fp, ",\"type\":\"unicode-overflow\",\"value\":%"PRIi32, err->as.unicodeOverflow);
    }; break;
    case EEXPRERR_UNCLOSED_CODEPOINT: {
      fprintf(fp, ",\"type\":\"unclosed-codepoint\"");
    }; break;
    case EEXPRERR_BAD_STRING_CHAR: {
      fprintf(fp, ",\"type\":\"bad-string-char\",\"input\":");
      fdumpChar(fp, err->as.badStringChar);
    }; break;
    case EEXPRERR_MISSING_LINE_PICKUP: {
      fprintf(fp, ",\"type\":\"missing-line-pickup\"");
    }; break;
    case EEXPRERR_UNCLOSED_STRING: {
      fprintf(fp, ",\"type\":\"unclosed-string\"");
    }; break;
    case EEXPRERR_HEREDOC_BAD_OPEN: {
      fprintf(fp, ",\"type\":\"heredoc-bad-open\"");
    }; break;
    case EEXPRERR_HEREDOC_BAD_INDENT_DEFINITION: {
      fprintf(fp, ",\"type\":\"heredoc-bad-indent-definition\"");
    }; break;
    case EEXPRERR_HEREDOC_BAD_INDENTATION: {
      fprintf(fp, ",\"type\":\"heredoc-bad-indentation\"");
    }; break;
    case EEXPRERR_UNCLOSED_HEREDOC: {
      fprintf(fp, ",\"type\":\"unclosed-heredoc\"");
    }; break;
    case EEXPRERR_MIXED_INDENTATION: {
      fprintf(fp, ",\"type\":\"mixed-indentation\",\"established\":{\"codepoint\":");
      fdumpChar(fp, err->as.mixedIndentation.chr);
      fprintf(fp, ",\"loc\":{\"from\":{\"line\":%zu,\"col\":%zu},\"to\":{\"line\":%zu,\"col\":%zu}}}"
         , err->as.mixedIndentation.loc.start.line + 1
         , err->as.mixedIndentation.loc.start.col + 1
         , err->as.mixedIndentation.loc.end.line + 1
         , err->as.mixedIndentation.loc.end.col + 1
         );
    }; break;
    case EEXPRERR_TRAILING_SPACE: {
      fprintf(fp, ",\"type\":\"trailing-space\"");
    }; break;
    case EEXPRERR_NO_TRAILING_NEWLINE: {
      fprintf(fp, ",\"type\":\"no-trailing-newline\"");
    }; break;
    case EEXPRERR_SHALLOW_INDENT: {
      fprintf(fp, ",\"type\":\"shallow-indent\"");
    }; break;
    case EEXPRERR_OFFSIDES: {
      fprintf(fp, ",\"type\":\"offsides\"");
    }; break;
    case EEXPRERR_BAD_DOT: {
      fprintf(fp, ",\"type\":\"bad-dot\"");
    }; break;
    case EEXPRERR_CRAMMED_TOKENS: {
      fprintf(fp, ",\"type\":\"crammed-tokens\"");
    }; break;
    case EEXPRERR_UNBALANCED_WRAP: {
      fprintf(fp, ",\"type\":\"unbalanced-wrap\"");
      if (err->as.unbalancedWrap.type != UCHAR_NULL) {
        fprintf(fp, ",\"unclosed\":{\"open\":");
        fdumpChar(fp, err->as.unbalancedWrap.type);
        fprintf(fp, ",\"loc\":{\"from\":{\"line\":%zu,\"col\":%zu},\"to\":{\"line\":%zu,\"col\":%zu}}}"
           , err->as.unbalancedWrap.loc.start.line + 1
           , err->as.unbalancedWrap.loc.start.col + 1
           , err->as.unbalancedWrap.loc.end.line + 1
           , err->as.unbalancedWrap.loc.end.col + 1
           );
      }
      else {
        fprintf(fp, ",\"unopened\":true}");
      }
    }; break;
    case EEXPRERR_EXPECTING_NEWLINE_OR_DEDENT: {
      fprintf(fp, ",\"type\":\"expect-newline-or-dedent\"");
    }; break;
    case EEXPRERR_MISSING_TEMPLATE_EXPR: {
      fprintf(fp, ",\"type\":\"missing-template-expr\"");
    }; break;
    case EEXPRERR_MISSING_CLOSE_TEMPLATE: {
      fprintf(fp, ",\"type\":\"missing-close-template\"");
    }; break;
  }
  fprintf(fp, "}");
}

void fdumpTokenArray(FILE* fp, const char* indent, size_t n, token** arr) {
  if (n == 0) {
    fprintf(fp, " []");
  }
  else {
    char* separator = "[ ";
    for (size_t i = 0; i < n; ++i) {
      fprintf(fp, "\n%s%s", indent, separator);
      fdumpToken(fp, arr[i]);
      separator = ", ";
    }
    fprintf(fp, "\n%s]", indent);
  }
}

void fdumpEexprArray(FILE* fp, int indent, const dynarr_eexpr_p* arr) {
  if (arr->len == 0) {
    fprintf(fp, "[]");
  }
  else {
    char* separator = "[ ";
    for (size_t i = 0; i < arr->len; ++i) {
      fprintf(fp, "\n%*s%s", indent, "", separator);
      fdumpEexpr(fp, indent + 2, arr->data[i]);
      separator = ", ";
    }
    fprintf(fp, "\n%*s]", indent, "");
  }
}

void fdumpErrorArray(FILE* fp, const char* indent, size_t n, eexpr_error* arr) {
  if (n == 0) {
    fprintf(fp, " []");
  }
  else {
    char* separator = "[ ";
    for (size_t i = 0; i < n; ++i) {
      fprintf(fp, "\n%s%s", indent, separator);
      fdumpError(fp, &arr[i]);
      separator = ", ";
    }
    fprintf(fp, "\n%s]", indent);
  }
}

void fdumpLineIndex(FILE* fp, size_t len, size_t* offsets) {
  char separator = '[';
  for (size_t i = 0; i < len; ++i) {
    fprintf(fp, "%c%zu", separator, offsets[i]);
    separator = ',';
  }
  fprintf(fp, "]");
}
