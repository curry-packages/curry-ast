{- |
     Author  : Kai-Oliver Prott
     Version : October 2024

     Datatype and operations to handle Positions.
-}
{-# OPTIONS_FRONTEND -Wno-incomplete-patterns #-}

module Curry.Position where

data Position 
  = Position Int Int
  | NoPos
  deriving (Eq, Ord, Show, Read)

line :: Position -> Int
line (Position l _) = l

column :: Position -> Int
column (Position _ c) = c

-- | Distance between the row of the first and the second argument
rowDist :: Position -> Position -> Int
rowDist NoPos           NoPos           = 0
rowDist (Position {})   NoPos           = 0
rowDist NoPos           (Position {})   = 0
rowDist (Position r1 _) (Position r2 _) = r2 - r1

incr :: Position -> Int -> Position
incr NoPos              _ = NoPos
incr (Position row col) n = Position row (col + n)
