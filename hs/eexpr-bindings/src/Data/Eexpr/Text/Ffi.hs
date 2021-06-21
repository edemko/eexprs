{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Data.Eexpr.Text.Ffi
  (
  -- * Version Info
    versionMajor
  , versionMinor
  , versionPatch
  -- * Parsing and Extracting Results
  , CParserObj
  -- ** Initialize and Configure
  , sizeofParser
  , initDefault
  , setMixedSpace
  , setMixedNewlines
  , setTrailingSpace
  , setNoTrailingNewline
  , setBadDigitSeparator
  , setPauseAt
  -- ** Perform Parsing
  , parse
  , deinitParser
  -- ** Extract Results
  , nEexprs
  , eexprAt
  , delEexprs
  , nErrors
  , delErrors
  , nTokens
  , tokenAt
  , errorAt
  , nWarnings
  , warningAt
  , delWarnings
  -- ** Pausing During Parsing
  , pauseAfterStart
  , pauseAfterRawlex
  , pauseAfterCooklex
  , pauseAfterParse
  , doNotPause
  -- * Eexprs
  , CEexpr
  , eexprDel
  , eexprLocate
  , eexprType
  , eexprSymbol
  , eexprNumber
  , eexprString
  , eexprParen
  , eexprBrack
  , eexprBrace
  , eexprBlock
  , eexprPredot
  , eexprChain
  , eexprSpace
  , eexprEllipsis
  , eexprColon
  , eexprComma
  , eexprSemicolon
  , asSymbol
  , asNumber
  , asString
  , asParen
  , asBrack
  , asBrace
  , asBlock
  , asPredot
  , asChain
  , asSpace
  , asEllipsis
  , asColon
  , asComma
  , asSemicolon
  -- ** Numbers
  , sizeofBignum
  , isPositive
  , nBigDigits
  , bigDigits
  , radix
  , nFracDigits
  , isPositive_exp
  , nBigDigits_exp
  , bigDigits_exp
  -- ** String Templates
  , sizeofStrTemplate
  , head_nBytes
  , head_utf8str
  , tail_nSubexprs
  , tailAt_subexpr
  , tailAt_nBytes
  , tailAt_utf8str
  -- * Errors and Warnings
  , CError
  , errLocation
  , errType
  , errTypeBadBytes
  , errTypeBadChar
  , errTypeMixedSpace
  , errTypeMixedNewlines
  , errTypeBadDigitSeparator
  , errTypeMissingExponent
  , errTypeBadExponentSign
  , errTypeBadEscapeChar
  , errTypeBadEscapeCode
  , errTypeUnicodeOverflow
  , errTypeBadStringChar
  , errTypeMissingLinePickup
  , errTypeUnclosedString
  , errTypeUnclosedMultilineString
  , errTypeHeredocBadOpen
  , errTypeHeredocBadIndentDefinition
  , errTypeHeredocBadIndentation
  , errTypeMixedIndentation
  , errTypeTrailingSpace
  , errTypeNoTrailingNewline
  , errTypeShallowIndent
  , errTypeOffsides
  , errTypeBadDot
  , errTypeCrammedTokens
  , errTypeUnbalancedWrap
  , errTypeExpectingNewlineOrDedent
  , errTypeMissingTemplateExpr
  , errTypeMissingCloseTemplate
  -- TODO more error info, as needed
  -- * Locations
  , CLocation
  , sizeofLoc
  , locStartByte
  , locStartLine
  , locStartCol
  , locEndByte
  , locEndLine
  , locEndCol
  ) where

import Data.Word (Word8,Word32)
import Foreign.C.Types (CBool(..),CChar,CInt(..),CSize(..))
import Foreign.Ptr (Ptr)


foreign import capi "eexpr.h value EEXPR_VERSION_MAJOR" versionMajor :: CInt
foreign import capi "eexpr.h value EEXPR_VERSION_MINOR" versionMinor :: CInt
foreign import capi "eexpr.h value EEXPR_VERSION_PATCH" versionPatch :: CInt

------------ Parser Objects ------------

data CParserObj

foreign import capi "hs_eexpr.h value sizeofParser" sizeofParser :: CSize

foreign import ccall unsafe "eexpr_parserInitDefault" initDefault
  :: Ptr CParserObj -> IO ()

-- TODO set the pauseAt member
-- TODO set error levels

foreign import capi "hs_eexpr.h setMixedSpace" setMixedSpace
  :: Ptr CParserObj -> CBool -> IO ()
foreign import capi "hs_eexpr.h setMixedNewlines" setMixedNewlines
  :: Ptr CParserObj -> CBool -> IO ()
foreign import capi "hs_eexpr.h setTrailingSpace" setTrailingSpace
  :: Ptr CParserObj -> CBool -> IO ()
foreign import capi "hs_eexpr.h setNoTrailingNewline" setNoTrailingNewline
  :: Ptr CParserObj -> CBool -> IO ()
foreign import capi "hs_eexpr.h setBadDigitSeparator" setBadDigitSeparator
  :: Ptr CParserObj -> CBool -> IO ()


foreign import capi "eexpr.h value EEXPR_PAUSE_AFTER_START" pauseAfterStart :: CInt
foreign import capi "eexpr.h value EEXPR_PAUSE_AFTER_RAWLEX" pauseAfterRawlex :: CInt
foreign import capi "eexpr.h value EEXPR_PAUSE_AFTER_COOKLEX" pauseAfterCooklex :: CInt
foreign import capi "eexpr.h value EEXPR_PAUSE_AFTER_PARSE" pauseAfterParse :: CInt
foreign import capi "eexpr.h value EEXPR_DO_NOT_PAUSE" doNotPause :: CInt

foreign import capi "hs_eexpr.h setPauseAt" setPauseAt
  :: Ptr CParserObj -> CInt -> IO ()

foreign import ccall unsafe "eexpr_parser_deinit" deinitParser :: Ptr CParserObj -> IO ()


------------ Parsing and Obtaining Results ------------

foreign import ccall unsafe "eexpr_parse" parse
  :: Ptr CParserObj -- the opaque parser object
  -> CSize -- the length of the input string
  -> Ptr CChar -- a pinned input string
  -> IO ()


foreign import capi "hs_eexpr.h parser_nEexprs" nEexprs
  :: Ptr CParserObj
  -> IO CSize
foreign import capi "hs_eexpr.h parser_eexprAt" eexprAt
  :: Ptr CParserObj
  -> CSize
  -> IO (Ptr CEexpr)
foreign import capi "hs_eexpr.h parser_delEexprs" delEexprs
  :: Ptr CParserObj
  -> IO ()

foreign import capi "hs_eexpr.h parser_nTokens" nTokens
  :: Ptr CParserObj
  -> IO CSize
foreign import capi "hs_eexpr.h parser_tokenAt" tokenAt
  :: Ptr CParserObj
  -> CSize
  -> IO (Ptr CToken)

foreign import capi "hs_eexpr.h parser_nErrors" nErrors
  :: Ptr CParserObj
  -> IO CSize
foreign import capi "hs_eexpr.h parser_errorAt" errorAt
  :: Ptr CParserObj
  -> CSize
  -> IO (Ptr CError)
foreign import capi "hs_eexpr.h parser_delErrors" delErrors
  :: Ptr CParserObj
  -> IO ()

foreign import capi "hs_eexpr.h parser_nWarnings" nWarnings
  :: Ptr CParserObj
  -> IO CSize
foreign import capi "hs_eexpr.h parser_warningAt" warningAt
  :: Ptr CParserObj
  -> CSize
  -> IO (Ptr CError)
foreign import capi "hs_eexpr.h parser_delWarnings" delWarnings
  :: Ptr CParserObj
  -> IO ()


------------ Extracting Data About Eexprs ------------

data CEexpr -- eexpr

foreign import ccall unsafe "eexpr.h eexpr_del" eexprDel :: Ptr CEexpr -> IO ()
-- TODO eexpr_deinit

foreign import capi "eexpr.h value EEXPR_SYMBOL" eexprSymbol :: CInt
foreign import capi "eexpr.h value EEXPR_NUMBER" eexprNumber :: CInt
foreign import capi "eexpr.h value EEXPR_STRING" eexprString :: CInt
foreign import capi "eexpr.h value EEXPR_PAREN" eexprParen :: CInt
foreign import capi "eexpr.h value EEXPR_BRACK" eexprBrack :: CInt
foreign import capi "eexpr.h value EEXPR_BRACE" eexprBrace :: CInt
foreign import capi "eexpr.h value EEXPR_BLOCK" eexprBlock :: CInt
foreign import capi "eexpr.h value EEXPR_PREDOT" eexprPredot :: CInt
foreign import capi "eexpr.h value EEXPR_CHAIN" eexprChain :: CInt
foreign import capi "eexpr.h value EEXPR_SPACE" eexprSpace :: CInt
foreign import capi "eexpr.h value EEXPR_ELLIPSIS" eexprEllipsis :: CInt
foreign import capi "eexpr.h value EEXPR_COLON" eexprColon :: CInt
foreign import capi "eexpr.h value EEXPR_COMMA" eexprComma :: CInt
foreign import capi "eexpr.h value EEXPR_SEMICOLON" eexprSemicolon :: CInt


foreign import ccall unsafe "eexpr_getType" eexprType
  :: Ptr CEexpr -> CInt


foreign import ccall unsafe "eexpr_asSymbol" asSymbol
  :: Ptr CEexpr -> Ptr CSize -> Ptr (Ptr Word8) -> IO CBool

foreign import ccall unsafe "eexpr_asNumber" asNumber
  :: Ptr CEexpr -> Ptr CBignum -> IO CBool

foreign import ccall unsafe "eexpr_asString" asString
  :: Ptr CEexpr -> Ptr CStrTemplate -> IO CBool

foreign import ccall unsafe "eexpr_asParen" asParen
  :: Ptr CEexpr -> Ptr (Ptr CEexpr) -> IO CBool

foreign import ccall unsafe "eexpr_asBrack" asBrack
  :: Ptr CEexpr -> Ptr (Ptr CEexpr) -> IO CBool

foreign import ccall unsafe "eexpr_asBrace" asBrace
  :: Ptr CEexpr -> Ptr (Ptr CEexpr) -> IO CBool

foreign import ccall unsafe "eexpr_asBlock" asBlock
  :: Ptr CEexpr -> Ptr CSize -> Ptr (Ptr (Ptr CEexpr)) -> IO CBool

foreign import ccall unsafe "eexpr_asPredot" asPredot
  :: Ptr CEexpr -> Ptr (Ptr CEexpr) -> IO CBool

foreign import ccall unsafe "eexpr_asChain" asChain
  :: Ptr CEexpr -> Ptr CSize -> Ptr (Ptr (Ptr CEexpr)) -> IO CBool

foreign import ccall unsafe "eexpr_asSpace" asSpace
  :: Ptr CEexpr -> Ptr CSize -> Ptr (Ptr (Ptr CEexpr)) -> IO CBool

foreign import ccall unsafe "eexpr_asEllipsis" asEllipsis
  :: Ptr CEexpr -> Ptr (Ptr CEexpr) -> Ptr (Ptr CEexpr) -> IO CBool

foreign import ccall unsafe "eexpr_asColon" asColon
  :: Ptr CEexpr -> Ptr (Ptr CEexpr) -> Ptr (Ptr CEexpr) -> IO CBool

foreign import ccall unsafe "eexpr_asComma" asComma
  :: Ptr CEexpr -> Ptr CSize -> Ptr (Ptr (Ptr CEexpr)) -> IO CBool

foreign import ccall unsafe "eexpr_asSemicolon" asSemicolon
  :: Ptr CEexpr -> Ptr CSize -> Ptr (Ptr (Ptr CEexpr)) -> IO CBool


foreign import capi "hs_eexpr.h value sizeofLoc" sizeofLoc :: CSize

foreign import capi "hs_eexpr.h locatePtr" eexprLocate
  :: Ptr CEexpr -> Ptr CLocation -> IO ()


------------ Extracting Data About Tokens ------------

data CToken


------------ Extracting Data About Errors/Warnings ------------

data CError

foreign import capi "hs_eexpr.h errLocation" errLocation :: Ptr CError -> IO (Ptr CLocation)

foreign import capi "hs_eexpr.h errType" errType :: Ptr CError -> IO CInt

foreign import capi "eexpr.h value EEXPR_ERR_BAD_BYTES" errTypeBadBytes :: CInt
foreign import capi "eexpr.h value EEXPR_ERR_BAD_CHAR" errTypeBadChar :: CInt
foreign import capi "eexpr.h value EEXPR_ERR_MIXED_SPACE" errTypeMixedSpace :: CInt
foreign import capi "eexpr.h value EEXPR_ERR_MIXED_NEWLINES" errTypeMixedNewlines :: CInt
foreign import capi "eexpr.h value EEXPR_ERR_BAD_DIGIT_SEPARATOR" errTypeBadDigitSeparator :: CInt
foreign import capi "eexpr.h value EEXPR_ERR_MISSING_EXPONENT" errTypeMissingExponent :: CInt
foreign import capi "eexpr.h value EEXPR_ERR_BAD_EXPONENT_SIGN" errTypeBadExponentSign :: CInt
foreign import capi "eexpr.h value EEXPR_ERR_BAD_ESCAPE_CHAR" errTypeBadEscapeChar :: CInt
foreign import capi "eexpr.h value EEXPR_ERR_BAD_ESCAPE_CODE" errTypeBadEscapeCode :: CInt
foreign import capi "eexpr.h value EEXPR_ERR_UNICODE_OVERFLOW" errTypeUnicodeOverflow :: CInt
foreign import capi "eexpr.h value EEXPR_ERR_BAD_STRING_CHAR" errTypeBadStringChar :: CInt
foreign import capi "eexpr.h value EEXPR_ERR_MISSING_LINE_PICKUP" errTypeMissingLinePickup :: CInt
foreign import capi "eexpr.h value EEXPR_ERR_UNCLOSED_STRING" errTypeUnclosedString :: CInt
foreign import capi "eexpr.h value EEXPR_ERR_UNCLOSED_MULTILINE_STRING" errTypeUnclosedMultilineString :: CInt
foreign import capi "eexpr.h value EEXPR_ERR_HEREDOC_BAD_OPEN" errTypeHeredocBadOpen :: CInt
foreign import capi "eexpr.h value EEXPR_ERR_HEREDOC_BAD_INDENT_DEFINITION" errTypeHeredocBadIndentDefinition :: CInt
foreign import capi "eexpr.h value EEXPR_ERR_HEREDOC_BAD_INDENTATION" errTypeHeredocBadIndentation :: CInt
foreign import capi "eexpr.h value EEXPR_ERR_MIXED_INDENTATION" errTypeMixedIndentation :: CInt
foreign import capi "eexpr.h value EEXPR_ERR_TRAILING_SPACE" errTypeTrailingSpace :: CInt
foreign import capi "eexpr.h value EEXPR_ERR_NO_TRAILING_NEWLINE" errTypeNoTrailingNewline :: CInt
foreign import capi "eexpr.h value EEXPR_ERR_SHALLOW_INDENT" errTypeShallowIndent :: CInt
foreign import capi "eexpr.h value EEXPR_ERR_OFFSIDES" errTypeOffsides :: CInt
foreign import capi "eexpr.h value EEXPR_ERR_BAD_DOT" errTypeBadDot :: CInt
foreign import capi "eexpr.h value EEXPR_ERR_CRAMMED_TOKENS" errTypeCrammedTokens :: CInt
foreign import capi "eexpr.h value EEXPR_ERR_UNBALANCED_WRAP" errTypeUnbalancedWrap :: CInt
foreign import capi "eexpr.h value EEXPR_ERR_EXPECTING_NEWLINE_OR_DEDENT" errTypeExpectingNewlineOrDedent :: CInt
foreign import capi "eexpr.h value EEXPR_ERR_MISSING_TEMPLATE_EXPR" errTypeMissingTemplateExpr :: CInt
foreign import capi "eexpr.h value EEXPR_ERR_MISSING_CLOSE_TEMPLATE" errTypeMissingCloseTemplate :: CInt

------------ Location Data ------------

data CLocation

foreign import capi "hs_eexpr.h locStartByte" locStartByte :: Ptr CLocation -> IO CSize
foreign import capi "hs_eexpr.h locStartLine" locStartLine :: Ptr CLocation -> IO CSize
foreign import capi "hs_eexpr.h locStartCol" locStartCol :: Ptr CLocation -> IO CSize
foreign import capi "hs_eexpr.h locEndByte" locEndByte :: Ptr CLocation -> IO CSize
foreign import capi "hs_eexpr.h locEndLine" locEndLine :: Ptr CLocation -> IO CSize
foreign import capi "hs_eexpr.h locEndCol" locEndCol :: Ptr CLocation -> IO CSize


------------ Helper Types ------------

data CBignum

foreign import capi "hs_eexpr.h value sizeofBignum" sizeofBignum :: CSize

foreign import capi "hs_eexpr.h number_isPositive" isPositive :: Ptr CBignum -> IO CBool
foreign import capi "hs_eexpr.h number_nBigDigits" nBigDigits :: Ptr CBignum -> IO CSize
foreign import capi "hs_eexpr.h number_bigDigits" bigDigits :: Ptr CBignum -> IO (Ptr Word32)
foreign import capi "hs_eexpr.h number_radix" radix :: Ptr CBignum -> IO Word8
foreign import capi "hs_eexpr.h number_nFracDigits" nFracDigits :: Ptr CBignum -> IO Word32
foreign import capi "hs_eexpr.h number_isPositive_exp" isPositive_exp :: Ptr CBignum -> IO CBool
foreign import capi "hs_eexpr.h number_nBigDigits_exp" nBigDigits_exp :: Ptr CBignum -> IO CSize
foreign import capi "hs_eexpr.h number_bigDigits_exp" bigDigits_exp :: Ptr CBignum -> IO (Ptr Word32)

data CStrTemplate

foreign import capi "hs_eexpr.h value sizeofStrTemplate" sizeofStrTemplate :: CSize

foreign import capi "hs_eexpr.h string_head_nBytes" head_nBytes :: Ptr CStrTemplate -> IO CSize
foreign import capi "hs_eexpr.h string_head_utf8str" head_utf8str :: Ptr CStrTemplate -> IO (Ptr Word8)
foreign import capi "hs_eexpr.h string_tail_nSubexprs" tail_nSubexprs :: Ptr CStrTemplate -> IO CSize
foreign import capi "hs_eexpr.h string_tailAt_subexpr" tailAt_subexpr :: Ptr CStrTemplate -> CSize -> IO (Ptr CEexpr)
foreign import capi "hs_eexpr.h string_tailAt_nBytes" tailAt_nBytes :: Ptr CStrTemplate -> CSize -> IO CSize
foreign import capi "hs_eexpr.h string_tailAt_utf8str" tailAt_utf8str :: Ptr CStrTemplate -> CSize -> IO (Ptr Word8)
