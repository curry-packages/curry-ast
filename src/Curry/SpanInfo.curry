{- |
     Author  : Kai-Oliver Prott
     Version : August 2018

     Datatype and operations to handle SpanInfos.
-}
module Curry.SpanInfo where

import Curry.Span
import Curry.Position

data SpanInfo = SpanInfo Span [Span] -- ^ Span for the whole entity
                                     -- and a list of minor sub-spans,
                                     -- e.g. keywords.
              | NoSpanInfo
  deriving (Eq, Show, Read)

-- | A class for easy access to SpanInfos
class HasSpanInfo a where
  getSpanInfo  :: a -> SpanInfo
  setSpanInfo  :: SpanInfo -> a -> a
  updateEndPos :: a -> a
  updateEndPos = id

instance HasSpanInfo SpanInfo where
  getSpanInfo  = id
  setSpanInfo  = const

fromSrcSpan :: Span -> SpanInfo
fromSrcSpan sp = SpanInfo sp []

fromSrcSpanBoth :: Span -> SpanInfo
fromSrcSpanBoth sp = SpanInfo sp [sp]

getSrcSpan :: HasSpanInfo a => a -> Span
getSrcSpan a = case getSpanInfo a of
  NoSpanInfo   -> NoSpan
  SpanInfo s _ -> s

setSrcSpan :: HasSpanInfo a => Span -> a -> a
setSrcSpan s a = case getSpanInfo a of
  NoSpanInfo     -> setSpanInfo (SpanInfo s [])  a
  SpanInfo _ inf -> setSpanInfo (SpanInfo s inf) a

fromSrcInfoPoints :: [Span] -> SpanInfo
fromSrcInfoPoints = SpanInfo NoSpan

getSrcInfoPoints :: HasSpanInfo a => a -> [Span]
getSrcInfoPoints a = case getSpanInfo a of
  NoSpanInfo    -> []
  SpanInfo _ xs -> xs

setSrcInfoPoints :: HasSpanInfo a => [Span] -> a -> a
setSrcInfoPoints inf a = case getSpanInfo a of
  NoSpanInfo   -> setSpanInfo (SpanInfo NoSpan inf) a
  SpanInfo s _ -> setSpanInfo (SpanInfo s      inf) a

getStartPosition :: HasSpanInfo a => a -> Position
getStartPosition a =  case getSrcSpan a of
  NoSpan   -> NoPos
  Span s _ -> s

getSrcSpanEnd :: HasSpanInfo a => a -> Position
getSrcSpanEnd a = case getSpanInfo a of
  NoSpanInfo   -> NoPos
  SpanInfo s _ -> end s

setStartPosition :: HasSpanInfo a => Position -> a -> a
setStartPosition p a = case getSrcSpan a of
  NoSpan   -> setSrcSpan (Span p NoPos) a
  Span _ e -> setSrcSpan (Span p     e) a

setEndPosition :: HasSpanInfo a => Position -> a -> a
setEndPosition e a = case getSrcSpan a of
  NoSpan   -> setSrcSpan (Span NoPos e) a
  Span p _ -> setSrcSpan (Span p     e) a

spanInfo2Pos :: HasSpanInfo a => a -> Position
spanInfo2Pos = getStartPosition
