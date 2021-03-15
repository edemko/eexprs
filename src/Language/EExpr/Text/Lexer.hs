{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Language.EExpr.Text.Lexer
    ( parse
    , depth
    , isSymbolChar
    , stringEscapes
    ) where

import Prelude hiding (lines)

import Language.EExpr.Text.Lexer.Types

import Control.Monad (void)
import Data.Functor ((<&>))
import Data.Text (Text)
import Language.EExpr.Text.Lexer.Results (Result(..), Error(..), expect, mixedIndent, panic)
import Text.Lightyear (Lightyear, Consume(..), Branch(..), TextPos)

import qualified Data.Char as C
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Text.Lightyear as P


------------ Choices ------------

-- for more options, peek around starting at https://www.compart.com/en/unicode/category
isSymbolChar :: Char -> Bool
isSymbolChar c = good && defensive
    where
    defensive = c `notElem` ("\\# \t\n\r()[]{},.;:`\'\"" :: [Char])
    good = C.isLetter c || C.isDigit c || nonModifyingSymbol || c `elem` ("~!@$%^&*-_=+|<>/?" :: [Char])
    nonModifyingSymbol = case C.generalCategory c of
        C.MathSymbol -> True
        C.CurrencySymbol -> True
        _ -> False

stringEscapes :: [(Char, Parser 'Greedy (Text, Text))]
stringEscapes = (fromBasic <$> basicEscapes) ++ fancyEscapes
    where
    -- Produces the character expected, as well as a parser
    -- which is run after the character has matched.
    -- That parser produces the original after the character
    -- and the whole escape's semantics.
    fromBasic :: (Char, Char) -> (Char, Parser 'Greedy (Text, Text))
    fromBasic (c, sem) = (c, pure $ ("", T.singleton sem))
    basicEscapes :: [(Char, Char)]
    basicEscapes =
        [ ('\\', '\\')
        , ('0', '\0')
        , ('a', '\a')
        , ('b', '\b')
        , ('e', '\27')
        , ('f', '\f')
        , ('n', '\n')
        , ('r', '\r')
        , ('t', '\t')
        , ('v', '\v')
        , ('\'', '\'')
        , ('\"', '\"')
        , ('`', '`')
        ]
    fancyEscapes =
        -- TODO \x[0-9a-fA-F]{2}
        -- TODO \u[0-9a-fA-F]{4}
        -- TODO \U(10|0[0-9a-fA-F])[0-9a-fA-F]{4}
        -- WARNING: I am only allowing `\n` as a line separator; who wants to merge libraries with differing encodings for linesep?
        [ ('\n', do
            leading <- P.takeWhile (`elem` [' ', '\t'])
            resume <- P.char (expect ["'\\' to resume string after linebreak"]) '\\'
            pure (leading `T.snoc` resume , "")
          )
        , ('&', pure ("", ""))
        ]


------------ Drivers ------------

parse :: Text -> [Lexeme (Result 'Free)]
parse inp = case P.runLightyear wholeFile inp () of
    Right toks -> toks
    Left err -> error $ "Internal EExpr Error! Please report.\nLexer failed to recover: " ++ show err

depth :: TextPos -> Text -> Either Error Int
depth loc orig = P.runLightyearPos parseDepth orig loc ()
    where
    parseDepth :: Parser 'Greedy Int
    parseDepth = do
        spaces <- T.concat <$> P.many (simple <|> continue)
        P.endOfInput mixedIndent
        pure $ T.length spaces
        where
        simple = P.takeWhile1 (panic "spaces") (==' ')
        continue = "" <$ P.string (panic "line continue") "\\\n"


type Parser c a = Lightyear c () Text Error a

wholeFile :: Parser 'Greedy [Lexeme (Result 'Free)]
wholeFile = do
    toks <- many anyToken
    P.endOfInput (panic "broad lexer failed to reach end of input")
    pure toks

anyToken :: Parser 'Greedy (Lexeme (Result 'Free))
anyToken = P.choice $ NE.fromList
    [ fmap Ok <$> whitespace
    , number
    , symbol
    , P.fromAtomic colonSymbol -- WARNING this must come before `separator` to extract words that start with a colon
    , fmap Ok <$> comment
    , heredoc -- WARNING this must come before `string`, b/c they both start with `"`
    , string
    , fmap Ok <$> bracket
    , separator
    , errToken
    ]

errToken :: Parser 'Greedy (Lexeme (Result 'Free))
errToken = do
    loc <- P.getPosition
    c <- P.any (panic "errToken")
    pure L
        { loc
        , orig = T.singleton c
        , payload = Error $ BadChar loc c
        }

------------ Whitespace ------------

whitespace :: Parser 'Greedy (Lexeme (Token 'Free))
whitespace = do
    pos0 <- P.getPosition
    (tok, orig) <- inline <|> newline
    pure $ L
        { loc = pos0
        , orig
        , payload = tok
        }
    where
    inline = (UnknownSpace,) . T.concat <$> P.some (simpleInline <|> splitline)
    simpleInline = P.takeWhile1 (panic "simple whitespace") (`elem` [' ', '\t'])
    splitline = P.string (panic "split whitespace") "\\\n"
    newline = (UnknownNewline,) <$> P.string (panic "newline") "\n"


-- NOTE: I'm thinking that block comments are not so useful
-- Text editors don't deal well with nesting block comments, which is what I'd like if I wanted block comments

comment :: Parser 'Greedy (Lexeme (Token 'Free))
comment = do
    pos0 <- P.getPosition
    hash <- P.char (panic "comment") '#'
    text <- P.takeWhile (/= '\n')
    let orig = hash `T.cons` text
    pure $ L
        { loc = pos0
        , orig
        , payload = Comment
        }


------------ Syntax ------------

separator :: Parser 'Greedy (Lexeme (Result 'Free))
separator = do
    loc <- P.getPosition
    (orig, payload) <- P.choice $ NE.fromList
        [ (str, Ok sem) <$ (P.string (expect [T.unpack str]) str)
        | (sem, str) <- separators
        ]
    pure L{loc, orig, payload}
    where
    separators :: [(Token 'Free, Text)]
    separators =
        -- WARNING: each token must come after all tokens it prefixes
        [ (Separator Comma, ",")
        , (Separator Ellipsis, "..")
        , (UnknownDot, ".")
        , (Separator Semicolon, ";")
        , (UnknownColon, ":")
        ]

bracket :: Parser 'Greedy (Lexeme (Token 'Free))
bracket = do
    pos0 <- P.getPosition
    (tok, c) <- P.choice $ NE.fromList $ combiners <&> \(sem, o, c) ->
        let open = (Combiner Open sem,) <$> P.char (panic "open bracket") o
            close = (Combiner Close sem,) <$> P.char (panic "close bracket") c
        in open <|> close
    pure L
        { loc = pos0
        , orig = T.singleton c
        , payload = tok
        }

combiners :: [(Combiner 'Free, Char, Char)]
combiners = map (\(sem, [o,c]) -> (sem, o, c)) db
    where
    db =
        [ (Paren, "()")
        , (Brack, "[]")
        , (Brace, "{}")
        -- TODO a mix of fancier ones, or mixfixes
        -- I'm thinking that mixfixes handle one thing, but enclose and separate handles another.
        -- Thus, `operator none (_ + _) add` defines (i.e. `'operator' ('left'|'right'|'none') '(' '_'? (<name> '_')* <name>? ')' <name>`)
        -- wheras `comprehension ⟬ ⟭` would define a new parenthesis
        -- and `comprehension ⟬ , ⟭` would define one which has elements separated by commas
        -- I'm tempted to have even comprehension ⟬ , ; ⟭` which would have semicolon-separated lists of comma-separated elements
        ]

symbol :: Parser 'Greedy (Lexeme (Result 'Free))
symbol = do
    pos0 <- P.getPosition
    orig <- P.takeWhile1 (expect ["symbol character"]) isSymbolChar
    pure L
        { loc = pos0
        , orig
        , payload = Ok . Atom . SymAtom $ orig
        }

-- FIXME I think I might want to be less permissive with colons in symbols
-- colon is already used as a separator and to introduce indentation
-- perhaps I only want `::`?
colonSymbol :: Parser 'Atomic (Lexeme (Result 'Free))
colonSymbol = P.try $ do
    pos0 <- P.getPosition
    colon <- P.char (panic "colon-word") ':'
    rest <- P.takeWhile1 (panic "colon-word") (\c -> isSymbolChar c || c == ':')
    let orig = colon `T.cons` rest
    pure L
        { loc = pos0
        , orig
        , payload = Ok . Atom . SymAtom $ orig
        }

number :: Parser 'Greedy (Lexeme (Result 'Free))
number = do
    -- confirm we're looking at a number beforre parsing anything else
    void $ P.lookAhead $ P.try $ do
        _ <- P.option_ $ P.satisfy (panic "isNum sign") (`elem` ['+', '-'])
        P.satisfy (panic "isNum digit") C.isDigit
    pos0 <- P.getPosition
    sign <- P.option Nothing (Just <$> P.char (panic "+") '+') <|> (Just <$> P.char (panic "-") '-')
    -- TODO I've only implemented decimal integers
    -- TODO allow for underscores as digit group separators
    whole <- P.takeWhile1 (panic "parseNum digit") C.isDigit
    pure L
        { loc = pos0
        , orig = maybe "" T.singleton sign <> whole
        , payload = Ok . Atom . IntAtom $ fromSign sign * fromWhole whole
        }
    where
    fromSign (Just '-') = (-1)
    fromSign _ = 1
    fromWhole = read . T.unpack


------------ Strings ------------

-- FIXME I think I can simplify this implementation; check the syntax docs
heredoc :: Parser 'Greedy (Lexeme (Result 'Free))
heredoc = do
    pos0 <- P.getPosition
    (origOpen, fence) <- openHeredoc
    lines <- T.concat <$> P.many1 (firstLine fence) (bodyLines fence)
    closeE <- P.recover (P.fromAtomic $ endHeredoc fence)
    pure $ case closeE of
        Right origClose -> L
            { loc = pos0
            , orig = origOpen <> lines <> origClose
            , payload = Ok $ String Plain lines Plain
            }
        Left err -> L
            { loc = pos0
            , orig = origOpen <> lines
            , payload = Error err
            }
    where
    grabToLine :: Parser c Text
    grabToLine = P.takeWhile (/= '\n')
    openHeredoc :: Parser 'Greedy (Text, Text)
    openHeredoc = do
        quotes <- P.string (panic "start of heredoc") "\"\"\""
        fence <- P.takeWhile1 (expect ["heredoc delimiter"]) (\c -> C.isAlphaNum c || c `elem` ("-_" :: String))
        nl <- T.singleton <$> P.char (expect ["newline"]) '\n'
        pure (quotes <> fence <> nl, fence)
    firstLine :: Text -> Parser 'Greedy Text
    firstLine fence = do
        P.notFollowedBy (const $ Panic "heredoc fence (first line)") (endHeredocFirst fence)
        grabToLine
    bodyLines :: Text -> Parser 'Greedy Text
    bodyLines fence = do
        P.notFollowedBy (const $ Panic "heredoc fence") (endHeredoc fence)
        c <- P.char (expect ["end of heredoc"]) '\n'
        line <- grabToLine
        pure (T.cons c line)
    endHeredoc :: Text -> Parser 'Atomic Text
    endHeredoc fence = P.unsafeToAtomic $ P.string (expect ["end of heredoc"]) ("\n" <> fence <> "\"\"\"")
    endHeredocFirst :: Text -> Parser 'Atomic Text
    endHeredocFirst fence = P.unsafeToAtomic $ P.string (expect ["end of heredoc"]) (fence <> "\"\"\"")

string :: Parser 'Greedy (Lexeme (Result 'Free))
string = do
    pos0 <- P.getPosition
    (openChar, open) <- strTemplJoin
    (orig, body) <- biconcat <$> many stringSection
    closeE <- P.recover strTemplJoin
    pure $ case closeE of
        Right (closeChar, close) -> L
            { loc = pos0
            , orig = openChar <> orig <> closeChar
            , payload = Ok $ String open body close
            }
        Left err -> L
            { loc = pos0
            , orig = openChar <> orig
            , payload = Error err
            }

stringSection :: Parser 'Greedy (Text, Text)
stringSection = plain <|> escape
    where
    plain = dup <$> P.takeWhile1 (panic "plain string characters") (`notElem` ['\"', '`', '\\', '\r', '\n'])
    escape = do
        bs <- P.char (panic "string escape") '\\'
        let nextChars = fst <$> stringEscapes
        c <- P.satisfy (expect $ (:[]) <$> nextChars) (`elem` nextChars)
        case lookup c stringEscapes of
            Nothing -> error "internal error: an escape character was recognized without a corresponding escape parser"
            Just p -> do
                (orig, sem) <- p
                pure (bs `T.cons` c `T.cons` orig, sem)

strTemplJoin :: Parser 'Greedy (Text, StrTemplJoin)
strTemplJoin = do
    c <- P.satisfy (expect ["end of string", "start of splice"]) (`elem` ['\"', '`'])
    let semantic = case c of { '\"' -> Plain ; '`' -> Templ; _ -> error "Internal EExpr Error" }
    pure (T.singleton c, semantic)


dup :: a -> (a, a)
dup x = (x, x)

biconcat :: (Monoid a, Monoid b) => [(a, b)] -> (a, b)
biconcat [] = (mempty, mempty)
biconcat ((a, b) : rest) = (a <> restA, b <> restB)
    where (restA, restB) = biconcat rest
