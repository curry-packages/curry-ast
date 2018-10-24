{- |
     Author  : Kai-Oliver Prott
     Version : August 2018

     Datatype and operations to handle SpanInfos.
-}
module Curry.SpanInfo where

import Curry.Span

data SpanInfo = SpanInfo Span [Span] -- ^ Span for the whole entity
                                     -- and a list of minor sub-spans,
                                     -- e.g. keywords.
              | NoSpanInfo
  deriving (Eq, Show, Read)

-- | A class for easy access to SpanInfos
class HasSpanInfo a where
  getSpanInfo :: a -> SpanInfo
  setSpanInfo :: SpanInfo -> a -> a

instance HasSpanInfo SpanInfo where
  getSpanInfo = id
  setSpanInfo = const

-- | Get the span of the whole entity
getSrcSpan :: HasSpanInfo a
           => a    -- ^ The entity with SpanInfos
           -> Span -- ^ Whole span of that entity
getSrcSpan a = case getSpanInfo a of
  NoSpanInfo   -> NoSpan
  SpanInfo s _ -> s

-- | Get the src info points of the entity
getSrcInfoPoints :: HasSpanInfo a
                 => a     -- ^ The entity with SpanInfos
                 -> [Span] -- ^ SrcInfoPoints
getSrcInfoPoints a = case getSpanInfo a of
  NoSpanInfo    -> []
  SpanInfo _ ss -> ss