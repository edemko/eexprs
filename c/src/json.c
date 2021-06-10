#include "json.h"

#include <assert.h>
#include <inttypes.h>

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
      fdumpChar(fp, tok->as.codepoint.chr);
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
    case TOK_WRAPPER: {
      const char* family;
      switch (tok->as.wrapper.chr) {
        case '(': family = "paren"; break;
        case '[': family = "brace"; break;
        case '{': family = "bracket"; break;
        case '\n': family = "indent"; break;
        default: assert(false);
      }
      const char* open = tok->as.wrapper.isOpen ? "true" : "false";
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
    case TOK_FAKEFIX: {
      fprintf(fp, ",\"type\":\"fakefix\"");
    }; break;
    case TOK_SEMICOLON: {
      fprintf(fp, ",\"type\":\"semicolon\"");
    }; break;
    case TOK_COMMA: {
      fprintf(fp, ",\"type\":\"comma\"");
    }; break;
    case TOK_EOF: {
      fprintf(fp, ",\"type\":\"end-of-file\"");
    }; break;
    case TOK_COMMENT: {
      fprintf(fp, ",\"type\":\"comment\"");
    }; break;
    case TOK_UNKNOWN_SPACE: {
      fprintf(fp, ",\"type\":\"unknown-space\",\"codepoint\":%"PRIi32, tok->as.space.chr);
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

void fdumpLexErr(FILE* fp, const lexError* err) {
  fprintf(fp, "{\"loc\":{\"from\":{\"line\":%zu,\"col\":%zu},\"to\":{\"line\":%zu,\"col\":%zu}}"
         , err->loc.start.line + 1
         , err->loc.start.col + 1
         , err->loc.end.line + 1
         , err->loc.end.col + 1
         );
  switch (err->type) {
    case LEXERR_NOERROR: { assert(false); }; break;
    case LEXERR_BAD_BYTES: {
      fprintf(fp, ",\"type\":\"bad-bytes\"");
    }; break;
    case LEXERR_BAD_CHAR: {
      fprintf(fp, ",\"type\":\"bad-char\",\"input\":");
      fdumpChar(fp, err->as.badChar);
    }; break;
    case LEXERR_MIXED_SPACE: {
      fprintf(fp, ",\"type\":\"mixed-space\"");
    }; break;
    case LEXERR_MIXED_NEWLINES: {
      fprintf(fp, ",\"type\":\"mixed-newlines\"");
    }; break;
    case LEXERR_MISSING_FRACTIONAL_PART: {
      fprintf(fp, ",\"type\":\"missing-fractional-part\"");
    }; break;
    case LEXERR_BAD_DIGIT_SEPARATOR: {
      fprintf(fp, ",\"type\":\"bad-digit-separator\"");
    }; break;
    case LEXERR_MISSING_EXPONENT: {
      fprintf(fp, ",\"type\":\"missing-exponent\"");
    }; break;
    case LEXERR_BAD_EXPONENT_SIGN: {
      fprintf(fp, ",\"type\":\"bad-exponent-sign\"");
    }; break;
    case LEXERR_BAD_CODEPOINT: {
      fprintf(fp, ",\"type\":\"bad-codepoint\",\"input\":");
      fdumpChar(fp, err->as.badCodepoint);
    }; break;
    case LEXERR_BAD_ESCAPE_CHAR: {
      fprintf(fp, ",\"type\":\"bad-escape-char\",\"input\":");
      fdumpChar(fp, err->as.badEscapeChar);
    }; break;
    case LEXERR_BAD_ESCAPE_CODE: {
      fprintf(fp, ",\"type\":\"bad-escape-code\",\"input\":\"");
      for (size_t i = 0; i < 6; ++i) {
        fjsonEscapeChar(fp, err->as.badEscapeCode[i]);
      }
      fprintf(fp, "\"");
    }; break;
    case LEXERR_UNICODE_OVERFLOW: {
      fprintf(fp, ",\"type\":\"unicode-overflow\",\"value\":%"PRIi32, err->as.unicodeOverflow);
    }; break;
    case LEXERR_UNCLOSED_CODEPOINT: {
      fprintf(fp, ",\"type\":\"unclosed-codepoint\"");
    }; break;
    case LEXERR_BAD_STRING_CHAR: {
      fprintf(fp, ",\"type\":\"bad-string-char\",\"input\":");
      fdumpChar(fp, err->as.badStringChar);
    }; break;
    case LEXERR_MISSING_LINE_PICKUP: {
      fprintf(fp, ",\"type\":\"missing-line-pickup\"");
    }; break;
    case LEXERR_UNCLOSED_STRING: {
      fprintf(fp, ",\"type\":\"unclosed-string\"");
    }; break;
    case LEXERR_HEREDOC_BAD_OPEN: {
      fprintf(fp, ",\"type\":\"heredoc-bad-open\"");
    }; break;
    case LEXERR_HEREDOC_BAD_INDENT_DEFINITION: {
      fprintf(fp, ",\"type\":\"heredoc-bad-indent-definition\"");
    }; break;
    case LEXERR_HEREDOC_BAD_INDENTATION: {
      fprintf(fp, ",\"type\":\"heredoc-bad-indentation\"");
    }; break;
    case LEXERR_UNCLOSED_HEREDOC: {
      fprintf(fp, ",\"type\":\"unclosed-heredoc\"");
    }; break;
    case LEXERR_MIXED_INDENTATION: {
      fprintf(fp, ",\"type\":\"mixed-indentation\",\"established\":{\"codepoint\":");
      fdumpChar(fp, err->as.mixedIndentation.chr);
      fprintf(fp, ",\"loc\":{\"from\":{\"line\":%zu,\"col\":%zu},\"to\":{\"line\":%zu,\"col\":%zu}}}"
         , err->as.mixedIndentation.loc.start.line + 1
         , err->as.mixedIndentation.loc.start.col + 1
         , err->as.mixedIndentation.loc.end.line + 1
         , err->as.mixedIndentation.loc.end.col + 1
         );
    }; break;
    case LEXERR_TRAILING_SPACE: {
      fprintf(fp, ",\"type\":\"trailing-space\"");
    }; break;
    case LEXERR_NO_TRAILING_NEWLINE: {
      fprintf(fp, ",\"type\":\"no-trailing-newline\"");
    }; break;
  }
  fprintf(fp, "}");
}

void fdumpTokenStream(FILE* fp, const char* indent, const tokenStream* root) {
  if (root == NULL) {
    fprintf(fp, " []");
  }
  else {
    char* separator = "[ ";
    for (const tokenStream* strm = root; strm != NULL; strm = strm->next) {
      if (strm->here.transparent) { continue; }
      fprintf(fp, "\n%s%s", indent, separator);
      fdumpToken(fp, &strm->here);
      separator = ", ";
    }
    fprintf(fp, "\n%s]", indent);
  }
}


void fdumpLexErrStream(FILE* fp, const char* indent, const lexErrStream* root) {
  if (root == NULL) {
    fprintf(fp, " []");
  }
  else {
    char* separator = "[ ";
    for (const lexErrStream* strm = root; strm != NULL; strm = strm->next) {
      if (strm->here.type == LEXERR_NOERROR) { continue; }
      fprintf(fp, "\n%s%s", indent, separator);
      fdumpLexErr(fp, &strm->here);
      separator = ", ";
    }
    fprintf(fp, "\n%s]", indent);
  }
}

void fdumpLineIndex(FILE* fp, const struct lineIndex* index) {
  char separator = '[';
  for (size_t i = 0; i < index->len; ++i) {
    fprintf(fp, "%c%zu", separator, index->offsets[i]);
    separator = ',';
  }
  fprintf(fp, "]");
}
