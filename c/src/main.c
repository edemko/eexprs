#include <assert.h>
#include <string.h>

#include "json.h"

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
    // TODO char* lineIndex;
    char* rawTokens;
    char* tokens;
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
  for (lexErrStream* strm = st->errStream; strm != NULL; ) {
    level l;
    switch (strm->here.type) {
      case LEXERR_MIXED_SPACE: { l = opts.levels.mixedSpace; } break;
      case LEXERR_MIXED_NEWLINES: { l = opts.levels.mixedNewlines; } break;
      case LEXERR_BAD_DIGIT_SEPARATOR: { l = opts.levels.badDigitSeparator; } break;
      case LEXERR_TRAILING_SPACE: { l = opts.levels.trailingSpace; } break;
      case LEXERR_NO_TRAILING_NEWLINE: { l = opts.levels.noTrailingNewline; } break;
      default: { l = ERROR; } break;
    }
    switch (l) {
      case ERROR: {}; break;
      case WARN: {
        lexErrStream* next = strm->next;
        lexer_errToWarn(st, strm);
        strm = next; continue;
      }; break;
      case IGNORE: {
        lexErrStream* next = strm->next;
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
  fdumpTokenStream(fp, "  ", st->outStream);
  fprintf(fp, "\n, \"warnings\":");
  fdumpLexErrStream(fp, "  ", st->warnStream);
  fprintf(fp, "\n, \"errors\":");
  fdumpLexErrStream(fp, "  ", st->errStream);
  fprintf(fp, "\n}\n");
  fclose(fp);
}

options parseOpts(int argc, char** argv) {
  options opts =
    { .inFilename = NULL
    , .dump =
      { .rawTokens = NULL
      , .tokens = NULL
      }
    , .levels =
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
        else if (!strcmp(argv[i], "dumpRawTokens")) { filename_p = &opts.dump.rawTokens; }
        else if (!strcmp(argv[i], "dumpTokens")) { filename_p = &opts.dump.tokens; }
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

  lexer st = lexer_newFromFile(opts.inFilename);
  // TODO dump original file (so that we have an actual ground-truth for displaying further errors &c)

  lexer_raw(&st);
  if (st.fatal != NULL) {
    lexer_addErr(&st, st.fatal);
    free(st.fatal);
    st.fatal = NULL;
  }
  relevelErrors(&st, opts);
  dumpLexer(opts.dump.rawTokens, &st, &opts);
  if (st.errStream != NULL) { goto finish; }

  lexer_cook(&st);
  relevelErrors(&st, opts);
  dumpLexer(opts.dump.tokens, &st, &opts);
  // TODO make a dump
  if (st.errStream != NULL) { goto finish; }

  // TODO hookup parser pass

  // report warnings and errors, exiting if there are any errors
  finish:
  if (st.errStream != NULL || st.warnStream != NULL) {
    fprintf(stderr, "{ \"filename\": ");
    fdumpCStr(stderr, opts.inFilename);
    fprintf(stderr, "\n, \"warnings\":");
    fdumpLexErrStream(stderr, "  ", st.warnStream);
    if (st.errStream != NULL) {
      fprintf(stderr, "\n, \"errors\":");
      fdumpLexErrStream(stderr, "  ", st.errStream);
    }
    fprintf(stderr, "\n}\n");
  }
  return st.errStream == NULL ? 0 : 1;
}
