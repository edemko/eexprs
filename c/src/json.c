#include <assert.h>
#include <inttypes.h>

#include "json.h"


void fdumpStr(FILE* fp, str text) {
  fprintf(fp, "\"");
  while (true) {
    uchar c;
    size_t adv = peekUchar(&c, text);
    if (c == UCHAR_NULL) { break; }
    else if (c < 0 || c > 0x10FFFF) {
      text.len--;
      text.bytes++;
      continue;
    }
    else if ((c < 20) | (c == '\"') | (c == '\\')) {
      fprintf(fp, "\\u00%02X", (uint8_t)c);
    }
    else {
      fwrite(text.bytes, 1/*byte per element*/, adv/*many elements*/, fp);
    }
    text.len -= adv;
    text.bytes += adv;
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
      default: {
        fprintf(stderr, "unhandled token type in fdumpTokens\n");
        exit(255);
      }; break;
    }
    fprintf(fp, "}\n");
  }
}
