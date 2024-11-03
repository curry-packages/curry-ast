{- |
     Author  : Kai-Oliver Prott
     Version : October 2024

     Datatypes for identifiers from the curry-frontend
-}
module Curry.Ident where

import Curry.SpanInfo
import Curry.Span
import Curry.Position

-- | Identifier for modules, from the curry-frontend.
data ModuleIdent = ModuleIdent SpanInfo [String]
  deriving (Show, Read)

instance Eq ModuleIdent where
  ModuleIdent _ ss1 == ModuleIdent _ ss2 = ss1 == ss2

instance Ord ModuleIdent where
  compare (ModuleIdent _ ss1) (ModuleIdent _ ss2) = compare ss1 ss2

instance HasSpanInfo ModuleIdent where
  getSpanInfo     (ModuleIdent spi _ ) = spi
  setSpanInfo spi (ModuleIdent _   ss) = ModuleIdent spi ss
  updateEndPos i =
    setEndPosition (incr (getStartPosition i) (mIdentLength i - 1)) i

-- | Identifier, from the curry-frontend.
data Ident = Ident SpanInfo String Int
  deriving (Show, Read)

instance Eq Ident where
  Ident _ s1 id1 == Ident _ s2 id2 = (s1, id1) == (s2, id2)

instance Ord Ident where
  compare (Ident _ s1 id1) (Ident _ s2 id2) = compare (s1, id1) (s2, id2)

instance HasSpanInfo Ident where
  getSpanInfo (Ident spi _ _)= spi
  setSpanInfo spi (Ident _ s idt) = Ident spi s idt
  updateEndPos i = case i of
    Ident (SpanInfo _ [_,ss]) _ _
      -> setEndPosition (end ss) i
    _ -> setEndPosition (incr (getStartPosition i) (identLength i - 1)) i

-- | Qualified identifier, from the curry-frontend.
data QualIdent = QualIdent SpanInfo (Maybe ModuleIdent) Ident
  deriving (Show, Read)

instance Eq QualIdent where
  QualIdent _ mid1 idt1 == QualIdent _ mid2 idt2 = (mid1, idt1) == (mid2, idt2)

instance Ord QualIdent where
  compare (QualIdent _ mid1 idt1) (QualIdent _ mid2 idt2) =
    compare (mid1, idt1) (mid2, idt2)

instance HasSpanInfo QualIdent where
  getSpanInfo (QualIdent spi _ _ ) = spi
  setSpanInfo spi (QualIdent _ mid idt) = QualIdent spi mid idt
  updateEndPos i = case i of
    QualIdent (SpanInfo _ [_,ss]) _ _
      -> setEndPosition (end ss) i
    _ -> setEndPosition (incr (getStartPosition i) (qIdentLength i - 1)) i

qIdentLength :: QualIdent -> Int
qIdentLength (QualIdent _ (Just m) i) = identLength i + mIdentLength m
qIdentLength (QualIdent _ Nothing  i) = identLength i

mIdentLength :: ModuleIdent -> Int
mIdentLength a = length (concat (midQualifiers a))
               + length (midQualifiers a)

identLength :: Ident -> Int
identLength a = length (idName a)

-- | Remove the qualification of an 'QualIdent'
unqualify :: QualIdent -> Ident
unqualify (QualIdent _ _ i) = i

idName :: Ident -> String
idName (Ident _ n _) = n

midQualifiers :: ModuleIdent -> [String]
midQualifiers (ModuleIdent _ mid) = mid
