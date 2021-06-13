#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "lexer/util.h"
#include "app/json.h"

void die(const char* msg) {
  fprintf(stderr, "%s\n", msg);
  exit(1);
}

typedef enum level {
  IGNORE,
  WARN,
  ERROR
} level;
typedef struct options {
  char* inFilename;
  struct {
    char* original;
    char* lineIndex;
    char* rawTokens;
    char* tokens;
    char* eexprs;
  } dump;
  struct {
    level mixedSpace;
    level mixedNewlines;
    level badDigitSeparator;
    level trailingSpace;
    level noTrailingNewline;
  } levels;
} options;

void relevelErrors(lexer* st, options opts) {
  for (dllistNode_eexprError* strm = st->errStream.start; strm != NULL; /*increment in body*/) {
    level l;
    switch (strm->here.type) {
      case EEXPRERR_MIXED_SPACE: { l = opts.levels.mixedSpace; } break;
      case EEXPRERR_MIXED_NEWLINES: { l = opts.levels.mixedNewlines; } break;
      case EEXPRERR_BAD_DIGIT_SEPARATOR: { l = opts.levels.badDigitSeparator; } break;
      case EEXPRERR_TRAILING_SPACE: { l = opts.levels.trailingSpace; } break;
      case EEXPRERR_NO_TRAILING_NEWLINE: { l = opts.levels.noTrailingNewline; } break;
      default: { l = ERROR; } break;
    }
    switch (l) {
      case ERROR: {}; break;
      case WARN: {
        dllistNode_eexprError* next = strm->next;
        dllist_moveAfter_eexprError(&st->warnStream, NULL, &st->errStream, strm);
        strm = next; continue;
      }; break;
      case IGNORE: {
        dllistNode_eexprError* next = strm->next;
        if (strm->prev != NULL) { strm->prev->next = strm->next; }
        free(strm);
        strm = next; continue;
      }; break;
    }
    strm = strm->next; continue;
  }
}


void dumpLexer(char* filename, const lexer* st, const options* opts) {
  if (filename == NULL) { return; }
  FILE* fp = fopen(filename, "w");
  fprintf(fp, "{ \"filename\": ");
  fdumpCStr(fp, opts->inFilename);
  fprintf(fp, "\n, \"lineOffsets\": ");
  fdumpLineIndex(fp, &st->lineIndex);
  fprintf(fp, "\n, \"tokens\":");
  fdumpTokenStream(fp, "  ", st->tokStream.start);
  fprintf(fp, "\n, \"warnings\":");
  fdumpErrorStream(fp, "  ", st->warnStream.start);
  fprintf(fp, "\n, \"errors\":");
  fdumpErrorStream(fp, "  ", st->errStream.start);
  fprintf(fp, "\n}\n");
  fclose(fp);
}
void dumpParser(char* filename, const lexer* st, const options* opts) {
  if (filename == NULL) { return; }
  FILE* fp = fopen(filename, "w");
  fprintf(fp, "{ \"filename\": ");
  fdumpCStr(fp, opts->inFilename);
  fprintf(fp, "\n, \"lineOffsets\": ");
  fdumpLineIndex(fp, &st->lineIndex);
  fprintf(fp, "\n, \"eexprs\":");
  fdumpEexprArray(fp, 2, &st->eexprStream);
  fprintf(fp, "\n, \"warnings\":");
  fdumpErrorStream(fp, "  ", st->warnStream.start);
  fprintf(fp, "\n, \"errors\":");
  fdumpErrorStream(fp, "  ", st->errStream.start);
  fprintf(fp, "\n}\n");
  fclose(fp);
}

options parseOpts(int argc, char** argv) {
  options opts =
    { .inFilename = NULL
    , .dump =
      { .original = NULL
      , .lineIndex = NULL
      , .rawTokens = NULL
      , .tokens = NULL
      , .eexprs = NULL
      }
    , .levels =
      // NOTE I decide default levels based on whether it's possible for a script to automatically fix things.
      // If not it's an error, if so it's a warning.
      // In some cases, the fixup script might need some configuration (e.g. width of a tab).
      // { .badBytes = ERROR
      // , .badChar = ERROR
      { .mixedSpace = WARN
      , .mixedNewlines = WARN
      // , .missingFractionalPart = ERROR
      , .badDigitSeparator = ERROR
      // , .missingExponent = ERROR
      // , .badExponentSign = ERROR
      // , .badCodepoint = ERROR
      // , .badEscapeChar = ERROR
      // , .badEscapeCode = ERROR
      // , .unicodeOverflow = ERROR
      // , .unclosedCodepoint = ERROR
      // , .badStringChar = ERROR
      // , .missingLinePickup = ERROR
      // , .unclosedString = ERROR
      // , .heredocBadOpen = ERROR
      // , .heredocBadIndentDefinition = ERROR
      // , .heredocBadIndentation = ERROR
      // , .unclosedHeredoc = ERROR
      // , .mixedIndentation = ERROR
      , .trailingSpace = WARN
      , .noTrailingNewline = WARN
      // , .shallowIndent = ERROR
      // , .offsides = ERROR
      // , .badDot = ERROR
      // , .crammedTokens = ERROR
      // , .unbalancedWrap = ERROR
      // , .expectingNewlineOrDedent = ERROR
      // , .missingTemplateExpr = ERROR
      // , .missingCloseTemplate = ERROR
      }
    };
  for (int i = 1; i < argc; ++i) {
    size_t len = strlen(argv[i]);
    if (len >= 2 && argv[i][0] == '-') {
      if (argv[i][1] == 'i') {
        switch (argv[i][2]) {
          case '\0': {
            ++i; if (i >= argc) { die("missing input file"); }
          }; break;
          case '=': {
            argv[i] = &argv[i][3];
          }; break;
          default: {
            argv[i] = &argv[i][2];
          }; break;
        }
        goto setInputFile;
      }
      else if (argv[i][2] == 'd') {
        argv[i] = &argv[i][2];
        char** filename_p = NULL;
        if (false) { assert(false); }
        else if (!strcmp(argv[i], "original")) { filename_p = &opts.dump.original; }
        else if (!strcmp(argv[i], "dumpLineIndex")) { filename_p = &opts.dump.lineIndex; }
        else if (!strcmp(argv[i], "dumpRawTokens")) { filename_p = &opts.dump.rawTokens; }
        else if (!strcmp(argv[i], "dumpTokens")) { filename_p = &opts.dump.tokens; }
        else if (!strcmp(argv[i], "dumpEexprs")) { filename_p = &opts.dump.eexprs; }
        else {
          fprintf(stderr, "unrecognized dump stage %s\n", argv[i]);
          exit(1);
        }
        if (filename_p != NULL) {
          ++i; if (i >= argc) { die("missing target file"); }
          *filename_p = argv[i];
        }
      }
      else if (argv[i][1] == 'E' || argv[i][1] == 'W' || argv[i][1] == 'N') {
        level l;
        switch (argv[i][1]) {
          case 'E': l = ERROR; break;
          case 'W': l = WARN; break;
          case 'N': l = IGNORE; break;
          default: assert(false);
        }
        argv[i] = &argv[i][2];
             if (false) { assert(false); }
        else if (!strcmp(argv[i], "mixed-space")) { opts.levels.mixedSpace = l; }
        else if (!strcmp(argv[i], "mixed-newlines")) { opts.levels.mixedNewlines = l; }
        else if (!strcmp(argv[i], "bad-digit-separator")) { opts.levels.badDigitSeparator = l; }
        else if (!strcmp(argv[i], "trailing-space")) { opts.levels.trailingSpace = l; }
        else if (!strcmp(argv[i], "no-trailing-newline")) { opts.levels.noTrailingNewline = l; }
        else {
          fprintf(stderr, "unrecognized error type %s\n", argv[i]);
          exit(1);
        }
      }
      else {
        fprintf(stderr, "unrecognized option: %s\n", argv[i]);
        exit(1);
      }
    }
    else setInputFile: {
      if (opts.inFilename != NULL) { die("only one input file is supported"); }
      opts.inFilename = argv[i];
    }
  }
  if (opts.inFilename == NULL) { die("no input file"); }
  return opts;
}


int main(int argc, char** argv) {
  options opts = parseOpts(argc, argv);

  parser st = parser_newFromFile(opts.inFilename);
  if (st.allInput.bytes == NULL) {
    parser_del(&st);
    die("error opening input file for reading");
  }
  if (opts.dump.original != NULL) {
    FILE* fp = fopen(opts.dump.original, "w");
    fwrite(st.allInput.bytes, 1/*byte per element*/, st.allInput.len/*elements*/, fp);
    fclose(fp);
  }
  if (opts.dump.lineIndex != NULL) {
    FILE* fp = fopen(opts.dump.lineIndex, "w");
    fprintf(fp, "{ \"filename\":");
    fdumpCStr(stdout, opts.inFilename);
    fprintf(fp, "\n, \"lineIndex\":");
    fdumpLineIndex(fp, &st.lineIndex);
    fprintf(fp, "\n}");
    fclose(fp);
  }

  bool parsed = false;
  lexer_raw(&st);
  relevelErrors(&st, opts);
  if (st.fatal.type != EEXPRERR_NOERROR) {
    dllist_insertAfter_eexprError(&st.errStream, NULL, &st.fatal);
    st.fatal.type = EEXPRERR_NOERROR;
  }
  dumpLexer(opts.dump.rawTokens, &st, &opts);
  if (st.errStream.start != NULL) { goto finish; }

  lexer_cook(&st);
  relevelErrors(&st, opts);
  dumpLexer(opts.dump.tokens, &st, &opts);
  if (st.errStream.start != NULL) { goto finish; }

  assert(st.tokStream.start != NULL);
  parser_parse(&st);
  relevelErrors(&st, opts);
  if (st.fatal.type != EEXPRERR_NOERROR) {
    dllist_insertAfter_eexprError(&st.errStream, NULL, &st.fatal);
    st.fatal.type = EEXPRERR_NOERROR;
  }
  dumpParser(opts.dump.eexprs, &st, &opts);
  parsed = true;
  if (st.errStream.start != NULL) { goto finish; }

  // report warnings and errors, exiting if there are any errors
  finish:
  if (parsed && st.errStream.start == NULL) {
    fprintf(stdout, "{ \"filename\": ");
    fdumpCStr(stdout, opts.inFilename);
    fprintf(stdout, "\n, \"eexprs\":");
    fdumpEexprArray(stdout, 2, &st.eexprStream);
    if (st.warnStream.start != NULL) {
      fprintf(stdout, "\n, \"warnings\":");
      fdumpErrorStream(stdout, "  ", st.warnStream.start);
    }
    fprintf(stdout, "\n}\n");
  }
  if (st.errStream.start != NULL || st.warnStream.start != NULL) {
    fprintf(stderr, "{ \"filename\": ");
    fdumpCStr(stderr, opts.inFilename);
    fprintf(stderr, "\n, \"warnings\":");
    fdumpErrorStream(stderr, "  ", st.warnStream.start);
    if (st.errStream.start != NULL) {
      fprintf(stderr, "\n, \"errors\":");
      fdumpErrorStream(stderr, "  ", st.errStream.start);
    }
    fprintf(stderr, "\n}\n");
  }
  int exitCode = st.errStream.start == NULL ? 0 : 1;
  parser_del(&st);
  return exitCode;
}
