{-# LANGUAGE NamedFieldPuns #-}
module Text.Nest.Tokens.Megaparsec.Location where

import Text.Nest.Tokens (Location(..))
import Text.Megaparsec.Pos (SourcePos(..), mkPos, unPos)


toLocation :: (SourcePos, SourcePos) -> Location
toLocation (start, end) = Loc
    { file = sourceName start
    , fromLine = unPos $ sourceLine start
    , fromCol = unPos $ sourceColumn start
    , toLine = unPos $ sourceLine end
    , toCol = unPos $ sourceColumn end
    }

fromLocation :: Location -> (SourcePos, SourcePos)
fromLocation Loc{file,fromLine,fromCol,toLine,toCol} = (start, end)
    where
    start = SourcePos {sourceName = file, sourceLine = mkPos fromLine, sourceColumn = mkPos fromCol}
    end = SourcePos {sourceName = file, sourceLine = mkPos toLine, sourceColumn = mkPos toCol}
