#include "eexpr.h"

#define sizeofParser sizeof(eexpr_parser)

#define setMixedSpace(p, v) (((eexpr_parser*)p)->isError.mixedSpace = (v))
#define setMixedNewlines(p, v) (((eexpr_parser*)p)->isError.mixedNewlines = (v))
#define setTrailingSpace(p, v) (((eexpr_parser*)p)->isError.trailingSpace = (v))
#define setNoTrailingNewline(p, v) (((eexpr_parser*)p)->isError.noTrailingNewline = (v))
#define setBadDigitSeparator(p, v) (((eexpr_parser*)p)->isError.badDigitSeparator = (v))

#define setPauseAt(p, v) (((eexpr_parser*)p)->pauseAt = (v))

#define parser_nEexprs(x) (((eexpr_parser*)x)->nEexprs)
#define parser_eexprAt(x, i) (((eexpr_parser*)x)->eexprs[i])
#define parser_delEexprs(p) do { \
    free(((eexpr_parser*)p)->eexprs); \
    ((eexpr_parser*)p)->nEexprs = 0; \
    ((eexpr_parser*)p)->eexprs = NULL; \
  } while(false)

#define parser_nTokens(x) (((eexpr_parser*)x)->nTokens)
#define parser_tokenAt(x, i) (((eexpr_parser*)x)->tokens[i])

#define parser_nErrors(x) (((eexpr_parser*)x)->nErrors)
#define parser_errorAt(x, i) (&((eexpr_parser*)x)->errors[i])

#define parser_nWarnings(x) (((eexpr_parser*)x)->nWarnings)
#define parser_warningAt(x, i) (&((eexpr_parser*)x)->warnings[i])


//////////// Location ////////////

#define sizeofLoc sizeof(eexpr_loc)

#define locatePtr(e,l) (*(eexpr_loc*)l = eexpr_locate((eexpr*)e))

#define locStartByte(l) (((eexpr_loc*)l)->start.byte)
#define locStartLine(l) (((eexpr_loc*)l)->start.line)
#define locStartCol(l) (((eexpr_loc*)l)->start.col)
#define locEndByte(l) (((eexpr_loc*)l)->end.byte)
#define locEndLine(l) (((eexpr_loc*)l)->end.line)
#define locEndCol(l) (((eexpr_loc*)l)->end.col)


//////////// Helper Types ////////////

#define sizeofBignum sizeof(eexpr_number)

#define number_isPositive(n) (((eexpr_number*)n)->isPositive)
#define number_nBigDigits(n) (((eexpr_number*)n)->nBigDigits)
#define number_bigDigits(n) (((eexpr_number*)n)->bigDigits)
#define number_radix(n) (((eexpr_number*)n)->radix)
#define number_nFracDigits(n) (((eexpr_number*)n)->nFracDigits)
#define number_isPositive_exp(n) (((eexpr_number*)n)->isPositive_exp)
#define number_nBigDigits_exp(n) (((eexpr_number*)n)->nBigDigits_exp)
#define number_bigDigits_exp(n) (((eexpr_number*)n)->bigDigits_exp)

#define sizeofStrTemplate sizeof(eexpr_string)

#define string_head_nBytes(str) (((eexpr_string*)str)->head.nBytes)
#define string_head_utf8str(str) (((eexpr_string*)str)->head.utf8str)
#define string_tail_nSubexprs(str) (((eexpr_string*)str)->nSubexprs)
#define string_tailAt_subexpr(str,i) (((eexpr_string*)str)->tail[i].subexpr)
#define string_tailAt_nBytes(str,i) (((eexpr_string*)str)->tail[i].nBytes)
#define string_tailAt_utf8str(str,i) (((eexpr_string*)str)->tail[i].utf8str)
