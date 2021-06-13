#include "parameters.h"

#include <assert.h>
#include <string.h>


//////////////////////////////////// Numbers ////////////////////////

// base 2
uchar _leader2[] = {'b','B',UCHAR_NULL};
uchar _digits2[] = {'0','1',UCHAR_NULL};
uchar _exp2[] = {'b','B',UCHAR_NULL};
// base 8
uchar _leader8[] = {'o','O',UCHAR_NULL};
uchar _digits8[] = {'0','1','2','3','4','5','6','7',UCHAR_NULL};
uchar _exp8[] = {UCHAR_NULL}; // I don't know of any widespread agreement
// base 10
uchar _leader10[] = {UCHAR_NULL};
uchar _digits10[] = {'0','1','2','3','4','5','6','7','8','9',UCHAR_NULL};
uchar _exp10[] = {'e','E',UCHAR_NULL};
// base 12
uchar _leader12[] = {'z','Z',UCHAR_NULL}; // as in doZenal
uchar _digits12[] = { '0','1','2','3','4','5','6','7','8','9',0x218A/*↊*/,0x218B/*↋*/
                    , '0','1','2','3','4','5','6','7','8','9','X','E' // after the usage of the Dozenal Society of America when they use ASCII
                    , UCHAR_NULL};
uchar _exp12[] = {UCHAR_NULL}; // I don't know of any widespread agreement
// base 16
uchar _leader16[] = {'x','X',UCHAR_NULL};
uchar _digits16[] = { '0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f'
                    , '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'
                    , UCHAR_NULL
                    };
uchar _exp16[] = {'h','H',UCHAR_NULL};
// base 62, base64(url) are not included, since they aren't easily understood by humans. instead, interpret a string (preferrably at compiletime)

// all the bases, from most-to-least commonly-used
const radixParams radices[] =
  { { .radix = 10
    , .leaderLetters = _leader10
    , .digits = _digits10
    , .exponentLetters = _exp10
    }
  , { .radix = 16
    , .leaderLetters = _leader16
    , .digits = _digits16
    , .exponentLetters = _exp16
    }
  , { .radix = 2
    , .leaderLetters = _leader2
    , .digits = _digits2
    , .exponentLetters = _exp2
    }
  , { .radix = 8
    , .leaderLetters = _leader8
    , .digits = _digits8
    , .exponentLetters = _exp8
    }
  , { .radix = 12
    , .leaderLetters = _leader12
    , .digits = _digits12
    , .exponentLetters = _exp12
    }
  , { .radix = 0 }
  };

const radixParams* defaultRadix = &radices[0];

bool isDigit(const radixParams* radix, uchar c) {
  return ucharElem(c, radix->digits);
}

const uchar positiveSign = '+';
const uchar negativeSign = '-';
bool isSign(uchar c) {
  return c == positiveSign || c == negativeSign;
}


const uchar digitSep = '_';
const uchar digitPoint = '.';

const uchar genericExpLetter = '^';

const radixParams* decodeRadix(uchar c) {
  for (size_t i = 0; radices[i].radix != 0; ++i) {
    if (ucharElem(c, radices[i].leaderLetters)) {
      return &radices[i];
    }
  }
  return NULL;
}

uint8_t decodeDigit(const radixParams* radix, uchar c) {
  assert(isDigit(radix, c));
  size_t amt = ucharFind(c, radix->digits);
  while (amt > radix->radix) { amt -= radix->radix; }
  return amt;
}


//////////////////////////////////// Symbols ////////////////////////

/* here's the original Haskell guess
isSymbolChar :: Char -> Bool
isSymbolChar c = good && defensive
    where
    defensive = c `notElem` ("\\# \t\n\r()[]{},.;:`\'\"" :: [Char])
    good = C.isLetter c || C.isDigit c || nonModifyingSymbol || c `elem` ("~!@$%^&*-_=+|<>/?" :: [Char])
    nonModifyingSymbol = case C.generalCategory c of
        C.MathSymbol -> True
        C.CurrencySymbol -> True
        _ -> False
*/
bool isSymbolChar(uchar c) {
  static const uchar miscChars[] =
    { '_' // TODO more!
    , '+', '-' // these are special because they can also start a number
    , '\'' // I do allow primes, but not at the start of a symbol
    , UCHAR_NULL};
  return ('a' <= c && c <= 'z')
      || ('A' <= c && c <= 'Z')
      || ('0' <= c && c <= '9')
      || ucharElem(c, miscChars) // found c in the noted characters
      || c == 0x03BB // DEBUG
      ;
}
bool isSymbolStart(uchar cs[2]) {
  // if the first char is a plus/minus, then the second char must not start a digit
  if (isSign(cs[0])) {
    return !isDigit(defaultRadix, cs[1]);
  }
  // otherwise, the first char needs in a proper subset of the symbol characters
  else {
    uchar c = cs[0];
    return isSymbolChar(c)
        && !isDigit(defaultRadix, c)
        && c != '\'';
        ;
  }
}


//////////////////////////////////// Strings ////////////////////////

bool isCodepointDelim(uchar c) {
  return c == '\'';
}
bool isStringDelim(uchar c) {
  return (c == '\"') | (c == '`');
}

strSpliceType spliceType(uchar open, uchar close) {
  if (open == '\"') {
    if (close == '\"') { return STRSPLICE_PLAIN; }
    else if (close == '`') { return STRSPLICE_OPEN; }
  }
  else if (open == '`') {
    if (close == '\"') { return STRSPLICE_CLOSE; }
    else if (close == '`') { return STRSPLICE_MIDDLE; }
  }
  return STRSPLICE_CORRUPT;
}
uchar plainStringDelim = '\"';

bool isStringChar(uchar c) {
  return (0x20 <= c)
       & (c < 0x10FFFF)
       & (c != escapeLeader)
      && !isCodepointDelim(c)
      && !isStringDelim(c)
       ;
      // TODO I should probably rule out all non-printing characters
}

uchar escapeLeader = '\\';

struct stdEscape commonEscapes[] =
  { {'\\', '\\'}
  , {'\'', '\''}
  , {'\"', '\"'}
  , {'`' , '`'}
  , {'n' , '\n'}
  , {'r' , '\r'}
  , {'t' , '\t'}
  , {'0' , '\0'}
  , {'e' , '\x1B'}
  , {'a' , '\a'}
  , {'b' , '\b'}
  , {'f' , '\f'}
  , {'v' , '\v'}
  , {UCHAR_NULL, UCHAR_NULL}
  };
// don't allow to escape any character (i.e. `\z` === `z`), since escape should mean "something special is going on here", not "something might need to happen here, I dunno"
uchar nullEscape = '&';

uchar twoHexEscapeLeader = 'x';
uchar fourHexEscapeLeader = 'u';
uchar sixHexEscapeLeader = 'U';





//////////////////////////////////// Whitespace ////////////////////////

const uchar spaceChar = ' ';
const uchar tabChar = '\t';
bool isSpaceChar(uchar c) {
  return (spaceChar == c) | (tabChar == c);
}

bool isNewlineChar(uchar c) {
  return ('\n' == c)
       | ('\r' == c)
       | ('\x1E' == c) // FIXME I think accepting \x1E is unnecessary, adds complication, and probly slows the lexer a little
       ;
}

newlineType decodeNewline(uchar c[2]) {
  // taken from the table at https://en.wikipedia.org/wiki/Newline
  switch (c[0]) {
    case '\n': switch (c[1]) {
      case '\r': return NEWLINE_SPOOLED;
      default: return NEWLINE_UNIX;
    }
    case '\r': switch (c[1]) {
      case '\n': return NEWLINE_WINDOWS;
      default: return NEWLINE_C64; // C64, ZX Spectrum, Apple II, and so on
    }
    case '\x1E': return NEWLINE_QNX; // version < 4
    default: return NEWLINE_NONE;
  }
}
size_t newlineSize(newlineType nl) {
  switch (nl) {
    case NEWLINE_NONE: return 0;
    case NEWLINE_UNIX: return 1;
    case NEWLINE_WINDOWS: return 2;
    case NEWLINE_C64: return 1;
    case NEWLINE_SPOOLED: return 2;
    case NEWLINE_QNX: return 1;
    default: return 0;
  }
}
const char* encodeNewline(newlineType nl) {
  switch (nl) {
    case NEWLINE_NONE: return "";
    case NEWLINE_UNIX: return "\n";
    case NEWLINE_WINDOWS: return "\r\n";
    case NEWLINE_C64: return "\r";
    case NEWLINE_SPOOLED: return "\n\r";
    case NEWLINE_QNX: return "\x1E";
    default: return "";
  }
}

struct untilEol untilEol(str in) {
  struct untilEol out = { .bytes = 0, .uchars = 0 };
  while (true) {
    uchar c;
    size_t adv = peekUchar(&c, in);
    in.bytes += adv;
    in.len -= adv;
    if ( (c == '\n')
       | (c == '\r')
       | (c == '\x1E')
       | (c == UCHAR_NULL)
       | (c < 0)
       ) {
      return out;
    }
    else {
      out.bytes += adv;
      out.uchars += 1;
    }
  }
}


//////////////////////////////////// Punctuation ////////////////////////

wrapType isWrapChar(uchar c) {
  switch(c) {
    case '(': return WRAP_PAREN;
    case ')': return WRAP_PAREN;
    case '[': return WRAP_BRACK;
    case ']': return WRAP_BRACK;
    case '{': return WRAP_BRACE;
    case '}': return WRAP_BRACE;
    default: return WRAP_NULL;
  }
}
bool isOpenWrap(uchar c) {
  return (c == '(')
       | (c == '[')
       | (c == '{')
       ;
}

splitter decodeSplitter(uchar c[2]) {
  splitter out = {.bytes = 0, .uchars = 0, .type = SPLITTER_NONE};
  switch (c[0]) {
    case ':': {
      out.bytes = out.uchars = 1;
      out.type = SPLITTER_COLON;
    }; break;
    case '.': switch (c[1]) {
      case '.': {
        out.bytes = out.uchars = 2;
        out.type = SPLITTER_ELLIPSIS;
      }; break;
      default: {
        out.bytes = out.uchars = 1;
        out.type = SPLITTER_DOT;
      }; break;
    }; break;
    case ';': {
      out.bytes = out.uchars = 1;
      out.type = SPLITTER_SEMICOLON;
    }; break;
    case ',': {
      out.bytes = out.uchars = 1;
      out.type = SPLITTER_COMMA;
    }; break;
  }
  return out;
}


bool isSeparateChar(uchar c) {
  return (c == ':')
       | (c == ';')
       | (c == '.')
       | (c == ',')
       ;
}

//////////////////////////////////// Miscellaneous ////////////////////////

const uchar commentChar = '#';
