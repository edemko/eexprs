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

void fdumpTokens(FILE* fp, tokenStream* strm) {
  for (; strm != NULL; strm = strm->next) {
    token tok = strm->here;
    fprintf(fp, "{\"loc\":{\"from\":{\"line\":%zu,\"col\":%zu},\"to\":{\"line\":%zu,\"col\":%zu}}"
           , tok.loc.start.line + 1
           , tok.loc.start.col + 1
           , tok.loc.end.line + 1
           , tok.loc.end.col + 1
           );
    switch (tok.type) {
      case TOK_NUMBER: {
        {
          str tmp = bigint_toDecimal(tok.as.number.mantissa);
          fprintf(fp, ",\"type\":\"number\",\"%s\":", tok.as.number.fractionalDigits ? "value" : "mantissa");
          fdumpStr(fp, tmp);
          free(tmp.bytes);
        }
        if (tok.as.number.radix != 10) {
          fprintf(fp, ",\"radix\":%d", tok.as.number.radix);
        }
        if (tok.as.number.fractionalDigits != 0 || tok.as.number.exponent.len != 0) {
          fprintf(fp, ",\"exponent\":{");
          bool needsComma = false;
          if (tok.as.number.fractionalDigits != 0) {
            fprintf(fp, "%s\"fractional\":-%"PRIu32, needsComma ? "," : "", tok.as.number.fractionalDigits);
            needsComma = true;
          }
          if (tok.as.number.exponent.len != 0) {
            str tmp = bigint_toDecimal(tok.as.number.exponent);
            fprintf(fp, "%s\"explicit\":", needsComma ? "," : "");
            fdumpStr(fp, tmp);
            free(tmp.bytes);
          }
          fprintf(fp, "}");
        }
      }; break;
      case TOK_CODEPOINT: {
        fprintf(fp, ",\"type\":\"codepoint\",\"value\":");
        fdumpChar(fp, tok.as.codepoint.chr);
      }; break;
      case TOK_STRING: {
        fprintf(fp, ",\"type\":\"string\",\"text\":");
        fdumpStr(fp, tok.as.string.text);
        switch (tok.as.string.splice) {
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
        fdumpStr(fp, tok.as.symbol.text);
      }; break;
      case TOK_WRAPPER: {
        const char* family;
        switch (tok.as.wrapper.chr) {
          case '(': family = "paren"; break;
          case '[': family = "brace"; break;
          case '{': family = "bracket"; break;
          case '\n': family = "indent"; break;
          default: assert(false);
        }
        const char* open = tok.as.wrapper.isOpen ? "true" : "false";
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
      case TOK_COMMENT: {
        fprintf(fp, ",\"type\":\"comment\"");
      }; break;
      case TOK_UNKNOWN_SPACE: {
        fprintf(fp, ",\"type\":\"unknown-space\",\"codepoint\":%"PRIi32, tok.as.space.chr);
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
      case TOK_NONE: { assert(false); }; break;
    }
    fprintf(fp, "}\n");
  }
}

void fdumpLexErrs(FILE* fp, lexErrStream* strm) {
  for (; strm != NULL; strm = strm->next) {
    lexError err = strm->here;
    if (err.type == LEXERR_NOERROR) { continue; }
    fprintf(fp, "{\"loc\":{\"from\":{\"line\":%zu,\"col\":%zu},\"to\":{\"line\":%zu,\"col\":%zu}}"
           , err.loc.start.line + 1
           , err.loc.start.col + 1
           , err.loc.end.line + 1
           , err.loc.end.col + 1
           );
    switch (err.type) {
      case LEXERR_NOERROR: { assert(false); }; break;
      case LEXERR_BAD_BYTES: {
        fprintf(fp, ",\"type\":\"bad-bytes\"");
      }; break;
      case LEXERR_BAD_CHAR: {
        fprintf(fp, ",\"type\":\"bad-char\",\"input\":");
        fdumpChar(fp, err.as.badChar);
      }; break;
      case LEXERR_MIXED_SPACE: {
        fprintf(fp, ",\"type\":\"mixed-space\"");
      }; break;
      case LEXERR_MIXED_NEWLINES: {
        fprintf(fp, ",\"type\":\"mixed-newlines\"");
      }; break;
      case LEXERR_BAD_CODEPOINT: {
        fprintf(fp, ",\"type\":\"bad-codepoint\",\"input\":");
        fdumpChar(fp, err.as.badCodepoint);
      }; break;
      case LEXERR_MISSING_FRACTIONAL_PART: {
        fprintf(fp, ",\"type\":\"missing-fractional-part\"");
      }; break;
      case LEXERR_BAD_DIGIT_SEPARATOR: {
        fprintf(fp, ",\"type\":\"bad-digit-separator\"");
      }; break;
      case LEXERROR_MISSING_EXPONENT: {
        fprintf(fp, ",\"type\":\"missing-exponent\"");
      }; break;
      case LEXERROR_BAD_EXPONENT_SIGN: {
        fprintf(fp, ",\"type\":\"bad-exponent-sign\"");
      }; break;
      case LEXERR_BAD_ESCAPE_CHAR: {
        fprintf(fp, ",\"type\":\"bad-escape-char\",\"input\":");
        fdumpChar(fp, err.as.badEscapeChar);
      }; break;
      case LEXERR_BAD_ESCAPE_CODE: {
        fprintf(fp, ",\"type\":\"bad-escape-code\",\"input\":\"");
        for (size_t i = 0; i < 6; ++i) {
          fjsonEscapeChar(fp, err.as.badEscapeCode[i]);
        }
        fprintf(fp, "\"");
      }; break;
      case LEXERR_UNICODE_OVERFLOW: {
        fprintf(fp, ",\"type\":\"unicode-overflow\",\"value\":%"PRIi32, err.as.unicodeOverflow);
      }; break;
      case LEXERR_UNCLOSED_CODEPOINT: {
        fprintf(fp, ",\"type\":\"unclosed-codepoint\"");
      }; break;
      case LEXERR_BAD_STRING_CHAR: {
        fprintf(fp, ",\"type\":\"bad-string-char\",\"input\":");
        fdumpChar(fp, err.as.badStringChar);
      }; break;
      case LEXERR_MISSING_LINE_PICKUP: {
        fprintf(fp, ",\"type\":\"missing-line-pickup\"");
      }; break;
      case LEXERR_UNCLOSED_STRING: {
        fprintf(fp, ",\"type\":\"unclosed-string\"");
      }; break;
    }
    fprintf(fp, "}\n");
  }
}
