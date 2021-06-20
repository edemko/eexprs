#include "json.h"

#include <assert.h>
#include <inttypes.h>
#include <stdlib.h>

#include "bigint.h"


bool needsJsonEscape(char32_t c) {
  return c < 0x20  || c == 0x7F // TODO encode other control codepoints
      || c == '\"' || c == '\\'
       ;
}
void fjsonEscapeChar(FILE* fp, char32_t c) {
  if (0x10FFFF < c) { return; }
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

void fdumpChar(FILE* fp, char32_t c) {
  fprintf(fp, "\"");
  fjsonEscapeChar(fp, c);
  fprintf(fp, "\"");
}
void fdumpStr(FILE* fp, str text) {
  fprintf(fp, "\"");
  while (text.len > 0) {
    char32_t c;
    size_t adv = peekUchar(&c, text);
    if (c < 0 || 0x10FFFF < c) {
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
void fdumpStrn(FILE* fp, size_t nBytes, uint8_t* utf8str) {
  str s = {.len = nBytes, .bytes = utf8str};
  fdumpStr(fp, s);
}
void fdumpCStr(FILE* fp, char* s) {
  str text = {.len = strlen(s), .bytes = (uint8_t*)s};
  fdumpStr(fp, text);
}

const char* wrapName(eexpr_wrapType type) {
  switch (type) {
    case EEXPR_WRAP_PAREN: return "paren";
    case EEXPR_WRAP_BRACE: return "brace";
    case EEXPR_WRAP_BRACK: return "bracket";
    case EEXPR_WRAP_BLOCK: return "indent";
    case EEXPR_WRAP_NULL: assert(false);
  }
  return "";
}

void fdumpToken(FILE* fp, const eexpr_token* tok) {
  eexpr_loc loc = eexpr_tokenLocate(tok);
  fprintf(fp, "{\"loc\":{\"from\":{\"line\":%zu,\"col\":%zu},\"to\":{\"line\":%zu,\"col\":%zu}}"
         , loc.start.line + 1
         , loc.start.col + 1
         , loc.end.line + 1
         , loc.end.col + 1
         );
  if (eexpr_tokenIsTransparent(tok)) {
    fprintf(fp, ",\"ignore\":true");
  }
  switch (eexpr_getTokenType(tok)) {
    case EEXPR_TOK_NUMBER: {
      eexpr_number num; eexpr_tokenAsNumber(tok, &num);
      {
        bigint mantissa = {.pos = num.isPositive, .len = num.nBigDigits, .buf = num.bigDigits};
        str tmp = bigint_toDecimal(mantissa);
        fprintf(fp, ",\"type\":\"number\",\"%s\":", num.nFracDigits ? "value" : "mantissa");
        fdumpStr(fp, tmp);
        free(tmp.bytes);
      }
      if (num.radix != 10) {
        fprintf(fp, ",\"radix\":%d", num.radix);
      }
      if (num.nFracDigits != 0 || num.nBigDigits_exp != 0) {
        fprintf(fp, ",\"exponent\":{");
        bool needsComma = false;
        if (num.nFracDigits != 0) {
          fprintf(fp, "%s\"fractional\":-%"PRIu32, needsComma ? "," : "", num.nFracDigits);
          needsComma = true;
        }
        if (num.nBigDigits_exp != 0) {
          bigint exponent = {.pos = num.isPositive_exp, .len = num.nBigDigits_exp, .buf = num.bigDigits_exp};
          str tmp = bigint_toDecimal(exponent);
          fprintf(fp, "%s\"explicit\":", needsComma ? "," : "");
          fdumpStr(fp, tmp);
          free(tmp.bytes);
        }
        fprintf(fp, "}");
      }
    }; break;
    case EEXPR_TOK_STRING: {
      eexpr_stringType type; size_t nBytes; uint8_t* utf8str;
      eexpr_tokenAsString(tok, &type, &nBytes, &utf8str);
      fprintf(fp, ",\"type\":\"string\",\"text\":");
      fdumpStrn(fp, nBytes, utf8str);
      switch (type) {
        case EEXPR_STRPLAIN: break;
        case EEXPR_STROPEN: {
          fprintf(fp, ",\"splice\":\"open\"");
        }; break;
        case EEXPR_STRMIDDLE: {
          fprintf(fp, ",\"splice\":\"middle\"");
        }; break;
        case EEXPR_STRCLOSE: {
          fprintf(fp, ",\"splice\":\"close\"");
        }; break;
        case EEXPR_STRCORRUPT: {
          fprintf(fp, ",\"splice\":\"corrupt\"");
        }; break;
      }
    }; break;
    case EEXPR_TOK_SYMBOL: {
      size_t nBytes; uint8_t* utf8str;
      eexpr_tokenAsSymbol(tok, &nBytes, &utf8str);
      fprintf(fp, ",\"type\":\"symbol\",\"text\":");
      fdumpStrn(fp, nBytes, utf8str);
    }; break;
    case EEXPR_TOK_WRAP: {
      eexpr_wrapType type; bool isOpen;
      eexpr_tokenAsWrap(tok, &type, &isOpen);
      const char* family = wrapName(type);
      const char* open = isOpen ? "true" : "false";
      fprintf(fp, ",\"type\":\"wrap\",\"family\":\"%s\",\"open\":%s", family, open);
    }; break;
    case EEXPR_TOK_COLON: {
      fprintf(fp, ",\"type\":\"colon\"");
    }; break;
    case EEXPR_TOK_ELLIPSIS: {
      fprintf(fp, ",\"type\":\"ellipsis\"");
    }; break;
    case EEXPR_TOK_CHAIN: {
      fprintf(fp, ",\"type\":\"chain\"");
    }; break;
    case EEXPR_TOK_PREDOT: {
      fprintf(fp, ",\"type\":\"predot\"");
    }; break;
    case EEXPR_TOK_SEMICOLON: {
      fprintf(fp, ",\"type\":\"semicolon\"");
    }; break;
    case EEXPR_TOK_COMMA: {
      fprintf(fp, ",\"type\":\"comma\"");
    }; break;
    case EEXPR_TOK_NEWLINE: {
      fprintf(fp, ",\"type\":\"newline\"");
    }; break;
    case EEXPR_TOK_SPACE: {
      fprintf(fp, ",\"type\":\"space\"");
    }; break;
    case EEXPR_TOK_EOF: {
      fprintf(fp, ",\"type\":\"end-of-file\"");
    }; break;
    case EEXPR_TOK_COMMENT: {
      fprintf(fp, ",\"type\":\"comment\"");
    }; break;
    case EEXPR_TOK_INDENT: {
      size_t depth;
      eexpr_tokenAsIndent(tok, &depth);
      fprintf(fp, ",\"type\":\"indent\",\"depth\":%zu", depth);
    }; break;
    case EEXPR_TOK_UNKNOWN_SPACE: {
      eexpr_spaceType type; size_t nChars;
      eexpr_tokenAsSpace(tok, &type, &nChars);
      char* typeDesc;
      switch (type) {
        case EEXPR_WSMIXED: typeDesc = ",\"mixed\":true"; break;
        case EEXPR_WSSPACES: typeDesc = ",\"char\":\" \""; break;
        case EEXPR_WSTABS: typeDesc = ",\"char\":\"\\t\""; break;
        case EEXPR_WSLINECONTINUE: typeDesc = ""; break;
      }
      fprintf(fp, ",\"type\":\"unknown-space\"%s,\"size\":%zu"
                , typeDesc
                , nChars
                );
    }; break;
    case EEXPR_TOK_UNKNOWN_NEWLINE: {
      fprintf(fp, ",\"type\":\"unknown-newline\"");
    }; break;
    case EEXPR_TOK_UNKNOWN_COLON: {
      fprintf(fp, ",\"type\":\"unknown-colon\"");
    }; break;
    case EEXPR_TOK_UNKNOWN_DOT: {
      fprintf(fp, ",\"type\":\"unknown-dot\"");
    }; break;
    case EEXPR_TOK_NUMBER_ERROR: {
      fprintf(fp, ",\"type\":\"error-number\"");
    }; break;
    case EEXPR_TOK_STRING_ERROR: {
      fprintf(fp, ",\"type\":\"error-string\"");
    }; break;
    case EEXPR_TOK_NONE: { assert(false); }; break;
  }
  fprintf(fp, "}");
}

void fdumpEexpr(FILE* fp, int indent, const eexpr* x) {
  eexpr_loc loc = eexpr_locate(x);
  fprintf(fp, "{ \"loc\":{\"from\":{\"line\":%zu,\"col\":%zu},\"to\":{\"line\":%zu,\"col\":%zu}}"
         , loc.start.line + 1
         , loc.start.col + 1
         , loc.end.line + 1
         , loc.end.col + 1
         );
  eexpr_type type = eexpr_getType(x);
  switch (type) {
    case EEXPR_SYMBOL: {
      size_t n; uint8_t* s; eexpr_asSymbol(x, &n, &s);
      fprintf(fp, "\n%*s, \"type\":\"symbol\",\"text\":", indent, "");
      fdumpStrn(fp, n, s);
    }; break;
    case EEXPR_NUMBER: {
      eexpr_number num; eexpr_asNumber(x, &num);
      {
        bigint mantissa = {.pos = num.isPositive, .len = num.nBigDigits, .buf = num.bigDigits};
        str tmp = bigint_toDecimal(mantissa);
        fprintf( fp, "\n%*s, \"type\":\"number\",\"%s\":"
               , indent, ""
               , num.nFracDigits == 0 ? "value" : "mantissa");
        fdumpStr(fp, tmp);
        free(tmp.bytes);
      }
      if (num.radix != 10) {
        fprintf(fp, ",\"radix\":%d", num.radix);
      }
      if (num.nFracDigits != 0 || num.nBigDigits_exp != 0) {
        fprintf(fp, ",\"exponent\":{");
        bool needsComma = false;
        if (num.nFracDigits != 0) {
          fprintf(fp, "%s\"fractional\":-%"PRIu32, needsComma ? "," : "", num.nFracDigits);
          needsComma = true;
        }
        if (num.nBigDigits_exp != 0) {
          bigint exponent = {.pos = num.isPositive_exp, .len = num.nBigDigits_exp, .buf = num.bigDigits_exp};
          str tmp = bigint_toDecimal(exponent);
          fprintf(fp, "%s\"explicit\":", needsComma ? "," : "");
          fdumpStr(fp, tmp);
          free(tmp.bytes);
        }
        fprintf(fp, "}");
      }
    }; break;
    case EEXPR_STRING: {
      eexpr_string s; eexpr_asString(x, &s);
      fprintf(fp, "\n%*s, \"type\":\"string\"", indent, "");
      if (s.nSubexprs == 0) {
        fprintf(fp, ",\"text\":");
        fdumpStrn(fp, s.head.nBytes, s.head.utf8str);
      }
      else {
        fprintf(fp, ",\"template\":\n%*s[ ", indent+2, "");
        fdumpStrn(fp, s.head.nBytes, s.head.utf8str);
        for (size_t i = 0; i < s.nSubexprs; ++i) {
          fprintf(fp, "\n%*s, ", indent+2, "");
          fdumpEexpr(fp, indent+4, s.tail[i].subexpr);
          fprintf(fp, "\n%*s, ", indent+2, "");
          fdumpStrn(fp, s.tail[i].nBytes, s.tail[i].utf8str);
        }
        fprintf(fp, "\n%*s]", indent+2, "");
      }
    }; break;
    case EEXPR_PAREN: {
      eexpr* y; eexpr_asParen(x, &y);
      fprintf(fp, "\n%*s, \"type\":\"paren\"", indent, "");
      if (y == NULL) {
        fprintf(fp, ",\"subexpr\":null");
      }
      else {
        fprintf(fp, ",\"subexpr\":\n%*s  ", indent, "");
        fdumpEexpr(fp, indent+2, y);
      }
    }; break;
    case EEXPR_BRACK: {
      eexpr* y; eexpr_asBrack(x, &y);
      fprintf(fp, "\n%*s, \"type\":\"bracket\"", indent, "");
      if (y == NULL) {
        fprintf(fp, ",\"subexpr\":null");
      }
      else {
        fprintf(fp, ",\"subexpr\":\n%*s  ", indent, "");
        fdumpEexpr(fp, indent+2, y);
      }
    }; break;
    case EEXPR_BRACE: {
      eexpr* y; eexpr_asBrace(x, &y);
      fprintf(fp, "\n%*s, \"type\":\"brace\"", indent, "");
      if (y == NULL) {
        fprintf(fp, ",\"subexpr\":null");
      }
      else {
        fprintf(fp, ",\"subexpr\":\n%*s  ", indent, "");
        fdumpEexpr(fp, indent+2, y);
      }
    }; break;
    case EEXPR_BLOCK: {
      size_t n; eexpr** ys; eexpr_asBlock(x, &n, &ys);
      fprintf(fp, "\n%*s, \"type\":\"block\",\"subexprs\":", indent, "");
      fdumpEexprArray(fp, indent+2, n, ys);
    }; break;
    case EEXPR_PREDOT: {
      eexpr* y; eexpr_asPredot(x, &y);
      fprintf(fp, "\n%*s, \"type\":\"predot\",\"subexpr\":", indent, "");
      fdumpEexpr(fp, indent+2, y);
    }; break;
    case EEXPR_CHAIN: {
      size_t n; eexpr** ys; eexpr_asChain(x, &n, &ys);
      fprintf(fp, "\n%*s, \"type\":\"chain\",\"subexprs\":", indent, "");
      fdumpEexprArray(fp, indent+2, n, ys);
    }; break;
    case EEXPR_SPACE: {
      size_t n; eexpr** ys; eexpr_asSpace(x, &n, &ys);
      fprintf(fp, "\n%*s, \"type\":\"space\",\"subexprs\":", indent, "");
      fdumpEexprArray(fp, indent+2, n, ys);
    }; break;
    case EEXPR_ELLIPSIS: {
      eexpr* before, *after; eexpr_asEllipsis(x, &before, &after);
      fprintf(fp, "\n%*s, \"type\":\"ellipsis\"", indent, "");
      fprintf(fp, "\n%*s, \"before\":", indent, "");
      if (before == NULL) {
        fprintf(fp, "null");
      }
      else {
        fprintf(fp, "\n%*s", indent+2, "");
        fdumpEexpr(fp, indent+2, before);
      }
      fprintf(fp, "\n%*s, \"after\":", indent, "");
      if (after == NULL) {
        fprintf(fp, "null");
      }
      else {
        fprintf(fp, "\n%*s", indent+2, "");
        fdumpEexpr(fp, indent+2, after);
      }
    }; break;
    case EEXPR_COLON: {
      eexpr* before, *after; eexpr_asColon(x, &before, &after);
      fprintf(fp, "\n%*s, \"type\":\"colon\",\"subexprs\":\n%*s[ ", indent, "", indent+2, "");
      fdumpEexpr(fp, indent+4, before);
      fprintf(fp, "\n%*s, ", indent+2, "");
      fdumpEexpr(fp, indent+4, after);
      fprintf(fp, "\n%*s]", indent+2, "");
    }; break;
    case EEXPR_COMMA: {
      size_t n; eexpr** ys; eexpr_asComma(x, &n, &ys);
      fprintf(fp, "\n%*s, \"type\":\"comma\",\"subexprs\":", indent, "");
      fdumpEexprArray(fp, indent+2, n, ys);
    }; break;
    case EEXPR_SEMICOLON: {
      size_t n; eexpr** ys; eexpr_asSemicolon(x, &n, &ys);
      fprintf(fp, "\n%*s, \"type\":\"semicolon\",\"subexprs\":", indent, "");
      fdumpEexprArray(fp, indent+2, n, ys);
    }; break;
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
    case EEXPR_ERR_NOERROR: { assert(false); }; break;
    case EEXPR_ERR_BAD_BYTES: {
      fprintf(fp, ",\"type\":\"bad-bytes\"");
    }; break;
    case EEXPR_ERR_BAD_CHAR: {
      fprintf(fp, ",\"type\":\"bad-char\",\"input\":");
      fdumpChar(fp, err->as.badChar);
    }; break;
    case EEXPR_ERR_MIXED_SPACE: {
      fprintf(fp, ",\"type\":\"mixed-space\"");
    }; break;
    case EEXPR_ERR_MIXED_NEWLINES: {
      fprintf(fp, ",\"type\":\"mixed-newlines\"");
    }; break;
    case EEXPR_ERR_BAD_DIGIT_SEPARATOR: {
      fprintf(fp, ",\"type\":\"bad-digit-separator\"");
    }; break;
    case EEXPR_ERR_MISSING_EXPONENT: {
      fprintf(fp, ",\"type\":\"missing-exponent\"");
    }; break;
    case EEXPR_ERR_BAD_EXPONENT_SIGN: {
      fprintf(fp, ",\"type\":\"bad-exponent-sign\"");
    }; break;
    case EEXPR_ERR_BAD_ESCAPE_CHAR: {
      fprintf(fp, ",\"type\":\"bad-escape-char\",\"input\":");
      fdumpChar(fp, err->as.badEscapeChar);
    }; break;
    case EEXPR_ERR_BAD_ESCAPE_CODE: {
      fprintf(fp, ",\"type\":\"bad-escape-code\",\"input\":\"");
      for (size_t i = 0; i < 6; ++i) {
        fjsonEscapeChar(fp, err->as.badEscapeCode[i]);
      }
      fprintf(fp, "\"");
    }; break;
    case EEXPR_ERR_UNICODE_OVERFLOW: {
      fprintf(fp, ",\"type\":\"unicode-overflow\",\"value\":%"PRIi32, err->as.unicodeOverflow);
    }; break;
    case EEXPR_ERR_BAD_STRING_CHAR: {
      fprintf(fp, ",\"type\":\"bad-string-char\",\"input\":");
      fdumpChar(fp, err->as.badStringChar);
    }; break;
    case EEXPR_ERR_MISSING_LINE_PICKUP: {
      fprintf(fp, ",\"type\":\"missing-line-pickup\"");
    }; break;
    case EEXPR_ERR_UNCLOSED_STRING: {
      fprintf(fp, ",\"type\":\"unclosed-string\"");
    }; break;
    case EEXPR_ERR_UNCLOSED_MULTILINE_STRING: {
      fprintf(fp, ",\"type\":\"unclosed-multiline-string\"");
    }; break;
    case EEXPR_ERR_MIXED_INDENTATION: {
    case EEXPR_ERR_HEREDOC_BAD_OPEN: {
      fprintf(fp, ",\"type\":\"heredoc-bad-open\"");
    }; break;
    case EEXPR_ERR_HEREDOC_BAD_INDENT_DEFINITION: {
      fprintf(fp, ",\"type\":\"heredoc-bad-indent-definition\"");
    }; break;
    case EEXPR_ERR_HEREDOC_BAD_INDENTATION: {
      fprintf(fp, ",\"type\":\"heredoc-bad-indentation\"");
    }; break;
      fprintf(fp, ",\"type\":\"mixed-indentation\",\"established\":{\"type\":");
      char32_t c;
      switch (err->as.mixedIndentation.establishedType) {
        case EEXPR_INDENT_SPACES: c = ' '; break;
        case EEXPR_INDENT_TABS: c = '\t'; break;
        case EEXPR_INDENT_NULL: assert(false); break;
      }
      fdumpChar(fp, c);
      fprintf(fp, ",\"loc\":{\"from\":{\"line\":%zu,\"col\":%zu},\"to\":{\"line\":%zu,\"col\":%zu}}}"
         , err->as.mixedIndentation.establishedAt.start.line + 1
         , err->as.mixedIndentation.establishedAt.start.col + 1
         , err->as.mixedIndentation.establishedAt.end.line + 1
         , err->as.mixedIndentation.establishedAt.end.col + 1
         );
    }; break;
    case EEXPR_ERR_TRAILING_SPACE: {
      fprintf(fp, ",\"type\":\"trailing-space\"");
    }; break;
    case EEXPR_ERR_NO_TRAILING_NEWLINE: {
      fprintf(fp, ",\"type\":\"no-trailing-newline\"");
    }; break;
    case EEXPR_ERR_SHALLOW_INDENT: {
      fprintf(fp, ",\"type\":\"shallow-indent\"");
    }; break;
    case EEXPR_ERR_OFFSIDES: {
      fprintf(fp, ",\"type\":\"offsides\"");
    }; break;
    case EEXPR_ERR_BAD_DOT: {
      fprintf(fp, ",\"type\":\"bad-dot\"");
    }; break;
    case EEXPR_ERR_CRAMMED_TOKENS: {
      fprintf(fp, ",\"type\":\"crammed-tokens\"");
    }; break;
    case EEXPR_ERR_UNBALANCED_WRAP: {
      fprintf(fp, ",\"type\":\"unbalanced-wrap\"");
      if (err->as.unbalancedWrap.type != EEXPR_WRAP_NULL) {
        fprintf(fp, ",\"unclosed\":{\"open\":\"%s\"", wrapName(err->as.unbalancedWrap.type));
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
    case EEXPR_ERR_EXPECTING_NEWLINE_OR_DEDENT: {
      fprintf(fp, ",\"type\":\"expect-newline-or-dedent\"");
    }; break;
    case EEXPR_ERR_MISSING_TEMPLATE_EXPR: {
      fprintf(fp, ",\"type\":\"missing-template-expr\"");
    }; break;
    case EEXPR_ERR_MISSING_CLOSE_TEMPLATE: {
      fprintf(fp, ",\"type\":\"missing-close-template\"");
    }; break;
  }
  fprintf(fp, "}");
}

void fdumpTokenArray(FILE* fp, const char* indent, size_t n, eexpr_token** arr) {
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

void fdumpEexprArray(FILE* fp, int indent, size_t n, eexpr** xs) {
  if (n == 0) {
    fprintf(fp, "[]");
  }
  else {
    char* separator = "[ ";
    for (size_t i = 0; i < n; ++i) {
      fprintf(fp, "\n%*s%s", indent, "", separator);
      fdumpEexpr(fp, indent + 2, xs[i]);
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
