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
uchar _digits12[] = {'0','1','2','3','4','5','6','7','8','9',0x218A/*↊*/,0x218B/*↋*/,UCHAR_NULL};
uchar _exp12[] = {UCHAR_NULL}; // I don't know of any widespread agreement
// base 16
uchar _leader16[] = {'x','X',UCHAR_NULL};
uchar _digits16[] = { '0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f'
                    , '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'
                    , UCHAR_NULL
                    };
uchar _exp16[] = {'h','H',UCHAR_NULL};
// TODO consider base 62, base64url

// all the bases, from most-to-least commonly-used
const baseParams bases[] =
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
  };

const baseParams* defaultBase = &bases[0];

bool isDigit(const baseParams* base, uchar c) {
  return ucharElem(c, base->digits);
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
  static const uchar miscChars[] = {'_', '+', '-', UCHAR_NULL};
  return ('a' <= c && c <= 'z')
      || ('A' <= c && c <= 'Z')
      || ('0' <= c && c <= '9')
      || ucharElem(c, miscChars) // found c in the noted characters
      || c == 0x03BB // DEBUG
      ;
}
bool isSymbolStart(uchar cs[2]) {
  uchar c;
  // if the first char isn't a plus/minus, then we need only look at the first char
  if (cs[0] != '+' && cs[0] != '-') {
    c = cs[0];
  }
  // a standalone +/- is a valid symbol
  else if (cs[1] == UCHAR_NULL) {
    return true;
  }
  // a +/- can start a symbol as long as the enxt char also could
  else {
   c = cs[1];
  }
  // symbol-starting chars (after mucking with +/-) are just symbol chars that can't start numbers
  return isSymbolChar(c)
      && !isDigit(defaultBase, c)
      ;
}


//////////////////////////////////// Whitespace ////////////////////////

bool isSpaceChar(uchar c) {
  return ' ' == c
      || '\t' == c
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

bool isWrapChar(uchar c) {
  return (c == '(')
       | (c == ')')
       | (c == '[')
       | (c == ']')
       | (c == '{')
       | (c == '}')
       ;
}
uchar openWrapper(uchar c) {
  switch (c) {
    case '(': return '(';
    case ')': return '(';
    case '[': return '[';
    case ']': return '[';
    case '{': return '{';
    case '}': return '{';
    default: assert(false);
  }
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
