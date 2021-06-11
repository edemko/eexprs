#include <assert.h>

#include "parser/util.h"


//////////////////////////////////// Individual Expression Parsers ////////////////////////////////////

bool parseNumber(parser* st) {
  token* tok = parser_peek(st);
  if (tok->type != TOK_NUMBER) { return false; }
  eexpr expr =
    { .loc = tok->loc
    , .type = EEXPR_NUMBER
    , .as.number = tok->as.number
    };
  parser_pop(st);
  parser_addEexpr(st, &expr);
  return true;
}

//////////////////////////////////// Main Parser ////////////////////////////////////

void parseLine(parser* st) {
  if (parseNumber(st)) { return; }
  else { assert(false); }
}

void parser_parse(parser* st) {
  bool atStart = true;
  while (true) {
    switch (parser_peek(st)->type) {
      case TOK_NEWLINE: {
        assert(!atStart);
        parser_pop(st);
        parseLine(st);
      }; break;
      case TOK_EOF: return;
      default: {
        assert(atStart);
        parseLine(st);
        atStart = false;
      } break;
    }
  }
}
