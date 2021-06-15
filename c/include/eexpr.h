#ifndef EEXPR_H
#define EEXPR_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <uchar.h>

// TODO what does this file declare/define, and why? i.e. give an overview of the library
// locations
// an abstract eexpr type
// an interface to an eexpr parser
// functions that extract data from eexprs (which presumably were created by the parser)


typedef struct eexpr_token eexpr_token; // FIXME I need to expose a good (minimal) token type

typedef struct eexpr eexpr;
typedef struct eexpr_error eexpr_error;


//////////////////////////////////// Producing Eexprs ////////////////////////////////////

//////////// First we have to pay homage to configuration data types. ////////////
//////////// Proceed to the next section for the juicy bits.


// internal data structures maintained by the parser
typedef struct eexpr_parserInternal eexpr_parserInternal;

// Aggregates eexpr parser options and outputs.
// For each of the in/out arrays, if the output size is zero, then the corresponding array is guaranteed not to have moved.
typedef struct eexpr_parser {
  // On input: the capacity of the `.eexprs` array in number of eexprs.
  // On output: The number of eexprs in the `.eexprs` array.
  size_t nEexprs;
  // Output member: An array holding (pointers to) parsed exprs.
  // Unlike `.errors` and `.warnings`, this array is allocated internally (which atm means malloc)
  // Thus, `.nEexprs` is ignores on input (for now!)
  eexpr** eexprs;
  // On input: the capacity of the `.tokens` array in number of tokens.
  // On output: The number of tokens in the `.tokens` array.
  size_t nTokens;
  // Output member: An array holding (pointers to) tokens.
  // These pointers are still owned by the parser, and should not be `free`d by the users of this library.
  // This array is `free`d once lexing ends (whether successfully or not).
  eexpr_token** tokens;
  // On input: the capacity of the `.errors` array in number of errors.
  // On output: The number of errors in the `.errors` array.
  size_t nErrors;
  // On input: An array into which parse errors should be stored when generated.
  // On output: An array holding generated parsing errors.
  eexpr_error* errors;
  // On input: the capacity of the `.warnings` array in number of warnings.
  // On output: The number of warnings in the `.warnings` array.
  size_t nWarnings;
  // On input: An array into which warnings should be stored when generated.
  // On output: An array holding generated warnings.
  eexpr_error* warnings;
  // TODO options to reduce some errors to warnings
  struct eexpr_parseErrorLevels {
    bool mixedSpace;
    bool mixedNewlines;
    bool trailingSpace;
    bool noTrailingNewline;
    bool badDigitSeparator;
    // NOTE if more fields are added here, remember to edit `eexpr_parserInitDefault`
  } isError;
  // Specify a stage of parsing to pause at.
  // Calling `eexpr_parse` on the same parser will resume the parsing from where it was left off.
  enum eexpr_parsePauseAt {
    EEXPR_PAUSE_AT_START,
    EEXPR_PAUSE_AFTER_RAWLEX,
    EEXPR_PAUSE_AFTER_COOKLEX,
    EEXPR_PAUSE_AFTER_PARSE,
    EEXPR_DO_NOT_PAUSE
  } pauseAt;
  // pointer to implementation
  eexpr_parserInternal* impl;
} eexpr_parser;

// Initialize a `eexpr_parser` with default settings (good enough for most purposes).
// No memory initially allocated for output.
// Emit warnings rather than errors whenever possible.
void eexpr_parserInitDefault(eexpr_parser* parser);


//////////// The juicy bits! ////////////

// Returns false and is deinitialized (see eexpr_parser_deinit) when `parser.allocator` failed to obtain memory.
// Calls to resume a paused parser need not supply `nBytes = 0` and `utf8Input = NULL` arguments.
bool eexpr_parse
  // Input configuration from and output parsed eexprs to this data structure
  // See the definition of `eexpr_parser` for more details.
  ( eexpr_parser* parser
  // number of bytes of input
  , size_t nBytes
  // pointer to utf8-encoded string of size `nBytes`, (not NUL-terminated)
  , uint8_t* utf8Input
  );

// Deallocate internal data structures used by a `eexpr_parser`.
// This does not free memory used by `.eexprs`, `.errors`, or `.warnings`.
// Calling this multiple times is idempotent.
void eexpr_parser_deinit(eexpr_parser* parser);


//////////////////////////////////// Consuming Eexprs ////////////////////////////////////

// I do not report filenames as part of a location.
// For one thing, the input may not be from a file (e.g. instead drawn from terminal input).
// Also, placing a pointer to the filename within these structres would complicate memory management.
// If you wish to report a filename along with a location, you will need to maintain the filename yourself.
// A special case of this is that an eexpr interpreter might include eexprs from multiple files, and so each eexpr might need its own filename as part of its location.
// If this is so, I recommend first pattern-matching an eexpr into a data type that represents the language being interpreted;
//   such a data type should be able to accomodate location data beyond what the eexpr library itself defines.

struct eexpr_locPoint {
  // Line offset within input; i.e. `.line = 0` means the first line of the file (or other input).
  size_t line;
  // Column offset within a line; i.e. `.col = 0` means the first column of the line.
  // Columns are counted as unicode codepoints.
  // NOTE This means the `.col` is not necessarily the same as the number of user-percieved characters or grapheme clusters!
  size_t col;
  // The byte offset from the start of input (zero-indexed again).
  size_t byte;
};

typedef struct eexpr_loc {
  struct eexpr_locPoint start;
  struct eexpr_locPoint end;
} eexpr_loc;


// TODO get location data

// this library always represents zero as `{false, 0, NULL}`; if providing a bigint to this library, be sure to use the same representation
// if `.words[i] == 0`, then there must exist `i < j < numWords` such that `.words[j] != 0`,
//   that is, `numWords` is always as small as possible
struct eexpr_bigint {
  bool isNegative;
  size_t numWords;
  // Pointer to the start of a little-endian array of at least `numWords` digits in base-2^32.
  // E.g. if `.numWords == 2`, then the natural number represented is `2^32 * .words[1] + .words[0]`.
  uint32_t* words;
};


// TODO eexpr_fromSymbol

// bool eexpr_fromNumber


//////////////////////////////////// Parse Errors ////////////////////////////////////

typedef enum eexpr_errorType {
  EEXPRERR_NOERROR, // only for use as a sentinel; an error of this type this should never be exposed
  // raw lexing errors
  EEXPRERR_BAD_BYTES,
  EEXPRERR_BAD_CHAR,
  EEXPRERR_MIXED_SPACE,
  EEXPRERR_MIXED_NEWLINES,
  EEXPRERR_BAD_DIGIT_SEPARATOR,
  EEXPRERR_MISSING_EXPONENT,
  EEXPRERR_BAD_EXPONENT_SIGN,
  EEXPRERR_BAD_ESCAPE_CHAR,
  EEXPRERR_BAD_ESCAPE_CODE,
  EEXPRERR_UNICODE_OVERFLOW,
  EEXPRERR_BAD_STRING_CHAR,
  EEXPRERR_MISSING_LINE_PICKUP,
  EEXPRERR_UNCLOSED_STRING,
  EEXPRERR_UNCLOSED_MULTILINE_STRING,
  EEXPRERR_HEREDOC_BAD_OPEN,
  EEXPRERR_HEREDOC_BAD_INDENT_DEFINITION,
  EEXPRERR_HEREDOC_BAD_INDENTATION,
  EEXPRERR_MIXED_INDENTATION,
  // cooking errors
  EEXPRERR_TRAILING_SPACE,
  EEXPRERR_NO_TRAILING_NEWLINE,
  EEXPRERR_SHALLOW_INDENT,
  EEXPRERR_OFFSIDES,
  EEXPRERR_BAD_DOT,
  EEXPRERR_CRAMMED_TOKENS,
  // parser errors
  EEXPRERR_UNBALANCED_WRAP,
  EEXPRERR_EXPECTING_NEWLINE_OR_DEDENT,
  EEXPRERR_MISSING_TEMPLATE_EXPR,
  EEXPRERR_MISSING_CLOSE_TEMPLATE
} eexpr_errorType;

typedef enum eexpr_wrapType {
  WRAP_NULL,
  WRAP_PAREN,
  WRAP_BRACK,
  WRAP_BRACE,
  WRAP_BLOCK
} eexpr_wrapType;

struct eexpr_error {
  eexpr_loc loc;
  eexpr_errorType type;
  union eexpr_errorInfo {
    char32_t badChar;
    char32_t badEscapeChar;
    char32_t badEscapeCode[6]; // if <6 `char32_t`s, then pad at start with U+0000
    uint32_t unicodeOverflow;
    char32_t badStringChar;
    struct eexpr_mixedIndentationInfo {
      char32_t chr; // FIXME use an indentation type enum
      eexpr_loc loc;
    } mixedIndentation;
    struct eexpr_unbalancedWrapInfo {
      eexpr_wrapType type; // what close wrap was left open, or WRAP_NULL for start-of-file
      eexpr_loc loc; // location where the unmatched open wrap is
    } unbalancedWrap;
  } as;
};


//////////////////////////////////// Tokens ////////////////////////////////////

typedef enum eexpr_spaceType {
  EEXPR_WSMIXED,
  EEXPR_WSSPACES,
  EEXPR_WSTABS,
  EEXPR_WSLINECONTINUE
} eexpr_spaceType;


#endif
