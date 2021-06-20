#ifndef EEXPR_H
#define EEXPR_H
/*
This defines an interface to an e-expression (eexpr) parser and data type.
The parser is configurable and can also be made to output tokens at an intermediate stages.
The eexpr and token data types are kept abstract; the data they hold can be accessed through a number of accessor functions.
The parser annotates every token and expression with its location in the source, both byte offset and line/col offset
  (column offsets count unicode codepoints, not grapheme clusters or user-perceived characters).

The parsing algorithm is in `eexpr_parse` and the key to its operation is an `eexpr_parser` value.
An ``eexpr_parser` is used to configure the parsing process, hold internal state during parsing, and present output data.
All of the inner state however is hidden behind a pointer to implementation.

The main output data if the parser are pointers to abstract `eexpr` and `eexpr_token` types.
The `eexpr_as*` and `eexpr_tokenAs*` families of functions pull out the constituent data of eexprs and tokens respectively.
Location data is obtained with `eexpr_locate` and `eexpr_tokenLocate`.

When eexpr data is no longer needed, it can be easily cleaned up with `eexpr_del` or `eexpr_deinit`.
Token data is inherently transient, and is cleaned up as soon as parsing completes.

Every identifier in this interface begins with either `eexpr_` or `EXPR_` (with the obvious exception of the `eexpr` type).
*/

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <uchar.h>


#define EEXPR_VERSION_MAJOR 0
#define EEXPR_VERSION_MINOR 1
#define EEXPR_VERSION_PATCH 1


typedef struct eexpr_token eexpr_token;

typedef struct eexpr eexpr;
typedef struct eexpr_error eexpr_error;


//////////////////////////////////// Producing Eexprs ////////////////////////////////////

//////////// First we have to pay homage to configuration data types. ////////////
//////////// Proceed to the next section for the juicy bits.


// internal data structures maintained by the parser
typedef struct eexpr_parserInternal eexpr_parserInternal;

// Aggregates eexpr parser options and outputs.
// For each of the in/out arrays, if the output size is zero, then the corresponding array is guaranteed not to have moved.
// If the input arrays are not null, they should be `malloc`d.
typedef struct eexpr_parser {
  // Output member: The number of eexprs in the `.eexprs` array.
  size_t nEexprs;
  // Output member: An array holding (pointers to) parsed exprs.
  // Initialize to `NULL` before parsing.
  // Once initialized, this array and its contents is owned by the owner of this struct.
  eexpr** eexprs;
  // On output: The number of tokens in the `.tokens` array.
  size_t nTokens;
  // Output member: An array holding (pointers to) tokens.
  // If non-null before parsing, this struct takes ownership of the array and will free it on `eexpr_deinit`.
  // This array's contents alias internal data structures and become invalid during the parsing stage.
  eexpr_token** tokens;
  // On input: the capacity of the `.errors` array in number of errors.
  // On output: The number of errors in the `.errors` array.
  size_t nErrors;
  // On input: An array into which parse errors should be stored when generated.
  // On output: An array holding generated lexing/parsing errors.
  // This array and its contents are owned by the owner of this struct.
  eexpr_error* errors;
  // On input: the capacity of the `.warnings` array in number of warnings.
  // On output: The number of warnings in the `.warnings` array.
  size_t nWarnings;
  // On input: An array into which warnings should be stored when generated.
  // On output: An array holding generated lexing/parsing warnings.
  // Like `.errors`, this array and its contents are owned by the owner of this struct.
  eexpr_error* warnings;
  // Some conditions can be treated as either errors or warnings.
  // When members of this struct are true, they are retained as errors, but when false (default) are demoted to warnings.
  // `eexpr_parser` refuses to continue parsing if there are any errors, but does not stop for warnings.
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
    EEXPR_PAUSE_AFTER_START,
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


// Lexes and parses input, reporting warnings/errors, and stopping on errors.
// It can be paused and resumed by appropriate configuration of `parser.pauseAt`, see the flowchart below.
// Returns true if parsing (up to the specified pause point) was successful.
/*
`eexpr_parserInitDefault(&parser)`
alter options
`eexpr_parse(&parser, len, inp)`
initialize                                            memory allocated for internal data structures
  |    |                                              inp is borrowed (i.e. must remain stable)
  |    \_________> if pause after start
  |                `parser.pauseAt = …`
  V                `eexpr_parse(&parser, 0, NULL)`
lexing stage  <_____________/
  |    |                                              error/warning outputs initialized
  |    \_________> if pause after rawlex              if pausing before parse, tokens output initialized
  |                `parser.pauseAt = …`
  V                `eexpr_parse(&parser, 0, NULL)`
post-lexing stage  <________/
  |    |                                              error/warning outputs initialized
  |    \_________> if pause after cooklex             if pausing before parse, tokens output initialized
  |                `parser.pauseAt = …`
  V                `eexpr_parse(&parser, 0, NULL)`
parsing stage  <____________/                         pointers obtained from tokens are now invalid
  |    |                                              error/warning outputs initialized
  |    \_________> if pause after parse               eexprs output initialized
  |                         V                         memory accessible from eexprs is owned (and uniquely referenced) by the caller
  |                `parser.pauseAt = …`
  V                `eexpr_parse(&parser, 0, NULL)`
finished  <_________________/
  |          ^---- `eexpr_parse(&parser, 0, NULL)`
  V
`eexpr_parser_deinit(&parser)`                        internal data structures are deinitialized
                                                      the parser can now be re-configured and re-used
*/
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


// Recursively free this eexpr and all its data.
void eexpr_del(eexpr* self);

// Recursively frees data used by the given eexpr, but does not free the eexpr itself.
void eexpr_deinit(eexpr* self);


typedef enum eexpr_type {
  EEXPR_SYMBOL,
  EEXPR_NUMBER,
  EEXPR_STRING,
  EEXPR_PAREN,
  EEXPR_BRACK,
  EEXPR_BRACE,
  EEXPR_BLOCK,
  EEXPR_PREDOT,
  EEXPR_CHAIN,
  EEXPR_SPACE,
  EEXPR_ELLIPSIS,
  EEXPR_COLON,
  EEXPR_COMMA,
  EEXPR_SEMICOLON
} eexpr_type;

// This is provided so that a if-elif-else or switch can quickly determine which of the `eexpr_as*` functions to call.
eexpr_type eexpr_getType(const eexpr* self);

/*
The `eexpr_as*` functions follow a common pattern:

  * The first argument is an eexpr to be analyzed.
  * If the eexpr's type matches the function, it returns true, otherwise false.
    For a given eexpr, exactly one `eexpr_as*` function will return true.
  * The following arguments are out parameters to which references to all of the data about the passed eexpr is written
    (except location, see `eexpr_locate`).

Note that the pointers returned from these functions are owned by the eexpr, and are never referenced from another eexpr.
Only *you* have the power to prevent forest fires^W^W^W alias these pointers.

Unless otherwise noted, the pointers input to or output from these functions are non-null.
*/

bool eexpr_asSymbol(const eexpr* self, size_t* nBytes, uint8_t** utf8str);

// Since numerical eexprs can easily outstrip the representational power of fixed-size machine formats,
//   eexprs have to represent these numbers as bignums.
// The numerical value is given by `significand * radix^(exp - nFracDigits)`;
//   see the individual members for calulating each of these parts.
// I've chosen `uint32_t` as the building block here because it makes it easy to perform carrying arithmetic:
//   operations between `uint32_t`s can fit in the standard-required `uint64_t`.
typedef struct eexpr_number {
  ////// Significand/Mantissa Information //////
  bool isPositive;
  // The number of base-2^32 digits needed to represent the significand (non-exponent part) of this number.
  size_t nBigDigits;
  // Array of base-2^32 digits representing the significand of this number, little-endian.
  // E.g. if `.nBigDigits == 2`, then the magnitude of the significand is represented is `2^32 * .bigDigits[1] + .bigDigits[0]`.
  // The smallest possible number of big digits is used. I.e. `.bigDigits[.nBigDigits - 1] != 0`.
  // If `.nBigDigits` is zero, then `.bigDigits` is `NULL`.
  uint32_t* bigDigits;
  // The base that the significand was represented with in the source code.
  // If there is no fractional or exponential part, then this field has only aesthetic value.
  uint8_t radix;
  // The number of base-`radix` digits come after the decimal point.
  // This is kept separate from info about the exponent so that consumers can detect
  //   if the number was written with or without a decimal point
  //   (e.g. so that downstream can decide to use integral/fractional representations).
  // If an eexpr source includes a number with over 4GiB of fractional digits, something has very likely gone wrong.
  // Well, if you really need it, it woudl be possible to make this a bignum too, it just complicates determining the effective exponent.
  uint32_t nFracDigits;
  ////// Exponent information //////
  bool isPositive_exp;
  // The number of base-2^32 digits needed to represent the exponent of this number.
  size_t nBigDigits_exp;
  // Array of base-32 digits needed to represent the exponent of this number, ni exactly the same format as `bigDigits`.
  // By exponent, I am referring only to the explicitly-written exponential part;
  //   the number of digits after the decimal point is not included here, but instead represented by `nFracDigits`.
  uint32_t* bigDigits_exp;
} eexpr_number;

bool eexpr_asNumber(const eexpr* self, eexpr_number* value);

typedef struct eexpr_string {
  // The first text part of the template.
  struct eexpr_strConst {
    size_t nBytes;
    uint8_t* utf8str;
  } head;
  // Length of the `tail` array in number of elements.
  size_t nSubexprs;
  // Array cnotaining additional subexpressions and text parts of the template.
  struct eexpr_strTemplate {
    // The subexpression to be spliced between the previous text part and `.utf8str`.
    eexpr* subexpr;
    // `.nBytes` and `.utf8str` together are the text part that should appear after `.subexpr`.
    size_t nBytes;
    uint8_t* utf8str;
  }* tail;
} eexpr_string;

bool eexpr_asString(const eexpr* self, eexpr_string* value);

// The output subexpr may be `NULL` for an empty parenthesized eexpr.
bool eexpr_asParen(const eexpr* self, eexpr** subexpr);

// The output subexpr may be `NULL` for an empty bracketed eexpr.
bool eexpr_asBrack(const eexpr* self, eexpr** subexpr);

// The output subexpr may be `NULL` for an empty braced eexpr.
bool eexpr_asBrace(const eexpr* self, eexpr** subexpr);

// Oof… three-star programming?
// It's okay, subexpr is an out parameter (star three) which will hold an array (star two) of boxed (star one) eexprs.
// So: `size_t n; eexpr** arr; eexpr_asBrace(e, n, arr);`
bool eexpr_asBlock(const eexpr* self, size_t* nSubexprs, eexpr*** subexprs);

bool eexpr_asPredot(const eexpr* self, eexpr** subexpr);

// Oof… three-star programming? Same explanation as for `eexpr_asBlock`.
bool eexpr_asChain(const eexpr* self, size_t* nSubexprs, eexpr*** subexprs);

// Oof… three-star programming? Same explanation as for `eexpr_asBlock`.
bool eexpr_asSpace(const eexpr* self, size_t* nSubexprs, eexpr*** subexprs);

// Either (or both) of the before and after outputs could be `NULL`.
bool eexpr_asEllipsis(const eexpr* self, eexpr** before, eexpr** after);

bool eexpr_asColon(const eexpr* self, eexpr** before, eexpr** after);

// Oof… three-star programming? Same explanation as for `eexpr_asBlock`.
bool eexpr_asComma(const eexpr* self, size_t* nSubexprs, eexpr*** subexprs);

// Oof… three-star programming? Same explanation as for `eexpr_asBlock`.
bool eexpr_asSemicolon(const eexpr* self, size_t* nSubexprs, eexpr*** subexprs);



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

// Return the location of an eexpr.
eexpr_loc eexpr_locate(const eexpr* self);


//////////////////////////////////// Parse Errors ////////////////////////////////////

typedef enum eexpr_errorType {
  EEXPR_ERR_NOERROR, // only for use as a sentinel; an error of this type this should never be exposed
  // raw lexing errors
  EEXPR_ERR_BAD_BYTES,
  EEXPR_ERR_BAD_CHAR,
  EEXPR_ERR_MIXED_SPACE,
  EEXPR_ERR_MIXED_NEWLINES,
  EEXPR_ERR_BAD_DIGIT_SEPARATOR,
  EEXPR_ERR_MISSING_EXPONENT,
  EEXPR_ERR_BAD_EXPONENT_SIGN,
  EEXPR_ERR_BAD_ESCAPE_CHAR,
  EEXPR_ERR_BAD_ESCAPE_CODE,
  EEXPR_ERR_UNICODE_OVERFLOW,
  EEXPR_ERR_BAD_STRING_CHAR,
  EEXPR_ERR_MISSING_LINE_PICKUP,
  EEXPR_ERR_UNCLOSED_STRING,
  EEXPR_ERR_UNCLOSED_MULTILINE_STRING,
  EEXPR_ERR_HEREDOC_BAD_OPEN,
  EEXPR_ERR_HEREDOC_BAD_INDENT_DEFINITION,
  EEXPR_ERR_HEREDOC_BAD_INDENTATION,
  EEXPR_ERR_MIXED_INDENTATION,
  // cooking errors
  EEXPR_ERR_TRAILING_SPACE,
  EEXPR_ERR_NO_TRAILING_NEWLINE,
  EEXPR_ERR_SHALLOW_INDENT,
  EEXPR_ERR_OFFSIDES,
  EEXPR_ERR_BAD_DOT,
  EEXPR_ERR_CRAMMED_TOKENS,
  // parser errors
  EEXPR_ERR_UNBALANCED_WRAP,
  EEXPR_ERR_EXPECTING_NEWLINE_OR_DEDENT,
  EEXPR_ERR_MISSING_TEMPLATE_EXPR,
  EEXPR_ERR_MISSING_CLOSE_TEMPLATE
} eexpr_errorType;

typedef enum eexpr_wrapType {
  EEXPR_WRAP_NULL, // only used internally
  EEXPR_WRAP_PAREN,
  EEXPR_WRAP_BRACK,
  EEXPR_WRAP_BRACE,
  EEXPR_WRAP_BLOCK
} eexpr_wrapType;

typedef enum eexpr_indentType {
  EEXPR_INDENT_NULL, // only used internally
  EEXPR_INDENT_SPACES,
  EEXPR_INDENT_TABS
} eexpr_indentType;

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
      eexpr_indentType establishedType;
      eexpr_loc establishedAt;
    } mixedIndentation;
    struct eexpr_unbalancedWrapInfo {
      eexpr_wrapType type; // what close wrap was left open, or WRAP_NULL for start-of-file
      eexpr_loc loc; // location where the unmatched open wrap is
    } unbalancedWrap;
  } as;
};


//////////////////////////////////// Tokens ////////////////////////////////////

typedef enum eexpr_tokenType {
  EEXPR_TOK_NUMBER,
  EEXPR_TOK_STRING,
  EEXPR_TOK_SYMBOL,
  EEXPR_TOK_WRAP,
  EEXPR_TOK_COLON,
  EEXPR_TOK_ELLIPSIS,
  EEXPR_TOK_CHAIN,
  EEXPR_TOK_PREDOT,
  EEXPR_TOK_SEMICOLON,
  EEXPR_TOK_COMMA,
  EEXPR_TOK_NEWLINE,
  EEXPR_TOK_SPACE,
  // tokens that will be dropped before parsing
  EEXPR_TOK_EOF,
  EEXPR_TOK_COMMENT,
  EEXPR_TOK_INDENT,
  // tokens that must later be resolved in context
  EEXPR_TOK_UNKNOWN_SPACE,
  EEXPR_TOK_UNKNOWN_NEWLINE,
  EEXPR_TOK_UNKNOWN_COLON,
  EEXPR_TOK_UNKNOWN_DOT,
  // tokens that are placeholders for bad syntax (to easy colorizing)
  EEXPR_TOK_NUMBER_ERROR,
  EEXPR_TOK_STRING_ERROR,
  // a sentinel token that doesn't make it into the token stream at all
  EEXPR_TOK_NONE
} eexpr_tokenType;

typedef enum eexpr_stringType {
  EEXPR_STRPLAIN,
  EEXPR_STROPEN,
  EEXPR_STRMIDDLE,
  EEXPR_STRCLOSE,
  EEXPR_STRCORRUPT
} eexpr_stringType;

typedef enum eexpr_spaceType {
  EEXPR_WSMIXED,
  EEXPR_WSSPACES,
  EEXPR_WSTABS,
  EEXPR_WSLINECONTINUE
} eexpr_spaceType;

eexpr_tokenType eexpr_getTokenType(const eexpr_token* self);

bool eexpr_tokenIsTransparent(const eexpr_token* self);

eexpr_loc eexpr_tokenLocate(const eexpr_token* self);


bool eexpr_tokenAsSymbol(const eexpr_token* self, size_t* nBytes, uint8_t** utf8str);

bool eexpr_tokenAsNumber(const eexpr_token* self, eexpr_number* value);

bool eexpr_tokenAsString(const eexpr_token* self, eexpr_stringType* type, size_t* nBytes, uint8_t** utf8str);

bool eexpr_tokenAsSpace(const eexpr_token* self, eexpr_spaceType* type, size_t* nChars);

bool eexpr_tokenAsIndent(const eexpr_token* self, size_t* depth);

bool eexpr_tokenAsWrap(const eexpr_token* self, eexpr_wrapType* type, bool* isOpen);


#endif
