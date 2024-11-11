{- |
     Author  : Kai-Oliver Prott
     Version : October 2024

     Datatype and operations to handle Spans.
-}
module Curry.Span (
 Span(..),
 -- * Transformer,
 isSpan, isNoSpan, fromPosition, stripStart, span2Pos, combineSpans, addSpan,
 -- * Distance management
 vertDist, isAfter, isBefore, isBeforeList, spanLength
) where

import Curry.Position

data Span 
  = Span 
    { start :: Position
    , end   :: Position
    }
  | NoSpan
    deriving (Eq, Show, Read)

isSpan :: Span -> Bool
isSpan (Span {})  = True
isSpan NoSpan     = False

isNoSpan :: Span -> Bool
isNoSpan (Span {})  = False
isNoSpan NoSpan     = True

-- | Create a Span with the given Position as start and end
fromPosition :: Position -> Span
fromPosition NoPos            = NoSpan
fromPosition p@(Position _ _) = Span p p

-- | Sets the start position of a Span to its end position
stripStart :: Span -> Span
stripStart = fromPosition . end

-- | Computes a "vertical distance" between two spans.
-- It is either the row distance of the start end end positions or
-- zero, if the spans overlap.
vertDist :: Span -> Span -> Int
vertDist NoSpan       NoSpan       = 0
vertDist NoSpan       (Span {})    = 0
vertDist (Span {})    NoSpan       = 0
vertDist (Span s1 e1) (Span s2 e2) =
  case rowDist e1 s2 of
    x | x >= 0    -> x
      | e1 <= e2  -> 0 -- they overlap
      | otherwise -> - (rowDist e2 s1)

-- | Checks if the first span is completely after the second span.
isAfter :: Span -> Span -> Bool
isAfter NoSpan     NoSpan     = False
isAfter (Span {})  NoSpan     = False
isAfter NoSpan     (Span {})  = False
isAfter (Span s _) (Span _ e) = s >= e

-- | Checks if the first span is completely before the second span.
isBefore :: Span -> Span -> Bool
isBefore NoSpan     NoSpan     = False
isBefore (Span {})  NoSpan     = False
isBefore NoSpan     (Span {})  = False
isBefore (Span _ e) (Span s _) = e <= s

-- | Checks if the first span is completely
--   before the first span of a list.
--   Reurns `True` if the list is empty
isBeforeList :: Span -> [Span] -> Bool
isBeforeList _   []      = True
isBeforeList sp1 (sp2:_) = isBefore sp1 sp2

span2Pos :: Span -> Position
span2Pos (Span p _) = p
span2Pos NoSpan     = NoPos

combineSpans :: Span -> Span -> Span
combineSpans sp1 sp2 = Span s e
  where s = start sp1
        e = end sp2

addSpan :: Span -> (a, [Span]) -> (a, [Span])
addSpan sp (a, ss) = (a, sp:ss)

spanLength :: Span -> (Int, Int)
spanLength sp = case sp of
  Span (Position x1 y1) (Position x2 y2)
    -> (x2-x1, y2-y1)
  _ -> (0, 0)
