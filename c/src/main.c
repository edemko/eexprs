#include "json.h"

int main(int argc, char** argv) {
  if (argc != 2) {
    fprintf(stderr, "expected one input file\n");
    return 1;
  }
  const char* filename = argv[1];
  lexer st = lexer_newFromFile(filename);
  lexer_raw(&st);
  fdumpTokens(stdout, st.outStream);
  fdumpLexErrs(stderr, st.errStream);
  return 0;
}
