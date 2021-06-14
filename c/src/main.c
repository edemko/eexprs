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

void relevelErrors(engine* st, options opts) {
  for (dllistNode_error* strm = st->errStream.start; strm != NULL; /*increment in body*/) {
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
        dllistNode_error* next = strm->next;
        dllist_moveAfter_error(&st->warnStream, NULL, &st->errStream, strm);
        strm = next; continue;
      }; break;
      case IGNORE: {
        dllistNode_error* next = strm->next;
        if (strm->prev != NULL) { strm->prev->next = strm->next; }
        free(strm);
        strm = next; continue;
      }; break;
    }
    strm = strm->next; continue;
  }
}


void dumpLexer(char* filename, const eexpr_parser* parser, const options* opts) {
  if (filename == NULL) { return; }
  FILE* fp = fopen(filename, "w");
  fprintf(fp, "{ \"filename\": ");
  fdumpCStr(fp, opts->inFilename);
  fprintf(fp, "\n, \"lineOffsets\": ");
  fdumpLineIndex(fp, parser->lines.len, parser->lines.offsets);
  fprintf(fp, "\n, \"tokens\":");
  fdumpTokenArray(fp, "  ", parser->nTokens, parser->tokens);
  fprintf(fp, "\n, \"warnings\":");
  fdumpErrorArray(fp, "  ", parser->nWarnings, parser->warnings);
  fprintf(fp, "\n, \"errors\":");
  fdumpErrorArray(fp, "  ", parser->nErrors, parser->errors);
  fprintf(fp, "\n}\n");
  fclose(fp);
}
void dumpParser(char* filename, const eexpr_parser* parser, const options* opts) {
  if (filename == NULL) { return; }
  FILE* fp = fopen(filename, "w");
  fprintf(fp, "{ \"filename\": ");
  fdumpCStr(fp, opts->inFilename);
  fprintf(fp, "\n, \"lineOffsets\": ");
  fdumpLineIndex(fp, parser->lines.len, parser->lines.offsets);
  fprintf(fp, "\n, \"eexprs\":");
  dynarr_eexpr_p fakeArr = {.len = parser->nEexprs, .cap = parser->nEexprs, .data = parser->eexprs};
  fdumpEexprArray(fp, 2, &fakeArr);
  fprintf(fp, "\n, \"warnings\":");
  fdumpErrorArray(fp, "  ", parser->nWarnings, parser->warnings);
  fprintf(fp, "\n, \"errors\":");
  fdumpErrorArray(fp, "  ", parser->nErrors, parser->errors);
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

  str input = readFile(opts.inFilename);
  if (input.bytes == NULL) {
    die("error opening input file for reading");
  }
  if (opts.dump.original != NULL) {
    FILE* fp = fopen(opts.dump.original, "w");
    fwrite(input.bytes, 1/*byte per element*/, input.len/*elements*/, fp);
    fclose(fp);
  }

  bool parsed = false;
  eexpr_parser parser; eexpr_parserInitDefault(&parser);

  parser.pauseAt = EEXPR_PAUSE_AFTER_RAWLEX;
  if (!eexpr_parse(&parser, input.len, input.bytes)) {
    die("out of memory");
  }
  if (opts.dump.lineIndex != NULL) {
    FILE* fp = fopen(opts.dump.lineIndex, "w");
    fprintf(fp, "{ \"filename\":");
    fdumpCStr(fp, opts.inFilename);
    fprintf(fp, "\n, \"lineIndex\":");
    fdumpLineIndex(fp, parser.lines.len, parser.lines.offsets);
    fprintf(fp, "\n}\n");
    fclose(fp);
  }
  dumpLexer(opts.dump.rawTokens, &parser, &opts);
  if (parser.nErrors != 0) { goto finish; }

  parser.pauseAt = EEXPR_PAUSE_AFTER_COOKLEX;
  if (!eexpr_parse(&parser, 0, NULL)) {
    die("out of memory");
  }
  dumpLexer(opts.dump.tokens, &parser, &opts);
  if (parser.nErrors != 0) { goto finish; }

  parser.pauseAt = EEXPR_DO_NOT_PAUSE;
  if (!eexpr_parse(&parser, 0, NULL)) {
    die("out of memory");
  }
  parsed = true;
  dumpParser(opts.dump.eexprs, &parser, &opts);

  // report warnings and errors, exiting if there are any errors
  finish:
  if (!parsed) { eexpr_parser_deinit(&parser); }
  if (parsed && parser.nErrors == 0) {
    fprintf(stdout, "{ \"filename\": ");
    fdumpCStr(stdout, opts.inFilename);
    fprintf(stdout, "\n, \"eexprs\":");
    dynarr_eexpr_p fakeArr = {.len = parser.nEexprs, .cap = parser.nEexprs, .data = parser.eexprs};
    fdumpEexprArray(stdout, 2, &fakeArr);
    if (parser.nWarnings != 0) {
      fprintf(stdout, "\n, \"warnings\":");
      fdumpErrorArray(stdout, "  ", parser.nWarnings, parser.warnings);
    }
    fprintf(stdout, "\n}\n");
  }
  if (parser.nErrors != 0 || parser.nWarnings != 0) {
    fprintf(stderr, "{ \"filename\": ");
    fdumpCStr(stderr, opts.inFilename);
    fprintf(stderr, "\n, \"warnings\":");
    fdumpErrorArray(stdout, "  ", parser.nWarnings, parser.warnings);
    if (parser.nErrors != 0) {
      fprintf(stderr, "\n, \"errors\":");
      fdumpErrorArray(stdout, "  ", parser.nErrors, parser.errors);
    }
    fprintf(stderr, "\n}\n");
  }
  for (size_t i = 0; i < parser.nEexprs; ++i) {
    eexpr_deinit(parser.eexprs[i]);
    free(parser.eexprs[i]);
  }
  free(parser.eexprs);
  return parser.nErrors == 0 ? 0 : 1;
}
