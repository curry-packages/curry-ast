{- |
     Author  : Kai-Oliver Prott
     Version : August 2018

     Datatypes for identifiers from the curry-frontend
-}
module Curry.Ident where

import Curry.Span
import Curry.SpanInfo
import Curry.Position

-- | Identifier for modules, from the curry-frontend.
data ModuleIdent = ModuleIdent SpanInfo [String]
  deriving (Show, Read)

instance Eq ModuleIdent where
  ModuleIdent _ ss1 == ModuleIdent _ ss2 = ss1 == ss2

instance Ord ModuleIdent where
  compare (ModuleIdent _ ss1) (ModuleIdent _ ss2) = compare ss1 ss2

instance HasSpanInfo ModuleIdent where
  getSpanInfo (ModuleIdent spi _)= spi
  setSpanInfo spi (ModuleIdent _ ss) = ModuleIdent spi ss
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
  updateEndPos i@(Ident (SpanInfo _ [_,ss]) _ _) =
    setEndPosition (end ss) i
  updateEndPos i =
    setEndPosition (incr (getStartPosition i) (identLength i - 1)) i

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
  updateEndPos i@(QualIdent (SpanInfo _ [_,ss]) _ _) =
    setEndPosition (end ss) i
  updateEndPos i =
    setEndPosition (incr (getStartPosition i) (qIdentLength i - 1)) i

idName :: Ident -> String
idName (Ident _ n _) = n

midQualifiers :: ModuleIdent -> [String]
midQualifiers (ModuleIdent _ mid) = mid

-- |Show function for an 'Ident'
showIdent :: Ident -> String
showIdent (Ident _ x n) | n == globalScope = x
                        | otherwise        = x ++ '.' : show n

renameIdent :: Ident -> Int -> Ident
renameIdent (Ident s n _) x = Ident s n x

-- |Revert the renaming of an 'Ident' by resetting its unique number
unRenameIdent :: Ident -> Ident
unRenameIdent ident = renameIdent ident globalScope

-- | Convert an 'Ident' to a 'QualIdent'
qualify :: Ident -> QualIdent
qualify i = QualIdent (getSpanInfo i) Nothing i

-- | Convert an 'Ident' to a 'QualIdent' with a given 'ModuleIdent'
qualifyWith :: ModuleIdent -> Ident -> QualIdent
qualifyWith mid i = updateEndPos $
  QualIdent (fromSrcSpan (getSrcSpan mid)) (Just mid) i

-- | Remove the qualification of an 'QualIdent'
unqualify :: QualIdent -> Ident
unqualify (QualIdent _ _ i) = i

-- |Global scope for renaming
globalScope :: Int
globalScope = 0

-- |Construct an 'Ident' from a 'String'
mkIdent :: String -> Ident
mkIdent x = Ident NoSpanInfo x globalScope

qIdentLength :: QualIdent -> Int
qIdentLength (QualIdent _ (Just m) i) = identLength i + mIdentLength m
qIdentLength (QualIdent _ Nothing  i) = identLength i

mIdentLength :: ModuleIdent -> Int
mIdentLength a = length (concat (midQualifiers a))
               + length (midQualifiers a)

identLength :: Ident -> Int
identLength a = length (idName a)

-- ---------------------------------------------------------------------------
-- A few identifiers are predefined here.
-- ---------------------------------------------------------------------------

-- | 'ModuleIdent' for the empty module
emptyMIdent :: ModuleIdent
emptyMIdent = ModuleIdent NoSpanInfo []

-- | 'ModuleIdent' for the main module
mainMIdent :: ModuleIdent
mainMIdent = ModuleIdent NoSpanInfo ["main"]

-- | 'ModuleIdent' for the Prelude
preludeMIdent :: ModuleIdent
preludeMIdent = ModuleIdent NoSpanInfo ["Prelude"]

-- ---------------------------------------------------------------------------
-- Identifiers for types
-- ---------------------------------------------------------------------------

-- | 'Ident' for the type '(->)'
arrowId :: Ident
arrowId = mkIdent "(->)"

-- | 'Ident' for the type/value unit ('()')
unitId :: Ident
unitId = mkIdent "()"

-- | 'Ident' for the type 'Bool'
boolId :: Ident
boolId = mkIdent "Bool"

-- | 'Ident' for the type 'Char'
charId :: Ident
charId = mkIdent "Char"

-- | 'Ident' for the type 'Int'
intId :: Ident
intId = mkIdent "Int"

-- | 'Ident' for the type 'Float'
floatId :: Ident
floatId = mkIdent "Float"

-- | 'Ident' for the type '[]'
listId :: Ident
listId = mkIdent "[]"

-- | 'Ident' for the type 'IO'
ioId :: Ident
ioId = mkIdent "IO"

-- | 'Ident' for the type 'Success'
successId :: Ident
successId = mkIdent "Success"

-- | Construct an 'Ident' for an n-ary tuple where n > 1
tupleId :: Int -> Ident
tupleId n
  | n > 1     = mkIdent $ '(' : replicate (n - 1) ',' ++ ")"
  | otherwise = error $ "Curry.Base.Ident.tupleId: wrong arity " ++ show n

-- | Check whether an 'Ident' is an identifier for an tuple type
isTupleId :: Ident -> Bool
isTupleId (Ident _ x _) = n > 1 && x == idName (tupleId n)
  where n = length x - 1

-- | Compute the arity of a tuple identifier
tupleArity :: Ident -> Int
tupleArity i@(Ident _ x _)
  | n > 1 && x == idName (tupleId n) = n
  | otherwise                        = error $
      "Curry.Base.Ident.tupleArity: no tuple identifier: " ++ showIdent i
  where n = length x - 1

-- ---------------------------------------------------------------------------
-- Identifiers for type classes
-- ---------------------------------------------------------------------------

-- | 'Ident' for the 'Eq' class
eqId :: Ident
eqId = mkIdent "Eq"

-- | 'Ident' for the 'Ord' class
ordId :: Ident
ordId = mkIdent "Ord"

-- | 'Ident' for the 'Enum' class
enumId :: Ident
enumId = mkIdent "Enum"

-- | 'Ident' for the 'Bounded' class
boundedId :: Ident
boundedId = mkIdent "Bounded"

-- | 'Ident' for the 'Read' class
readId :: Ident
readId = mkIdent "Read"

-- | 'Ident' for the 'Show' class
showId :: Ident
showId = mkIdent "Show"

-- | 'Ident' for the 'Num' class
numId :: Ident
numId = mkIdent "Num"

-- | 'Ident' for the 'Fractional' class
fractionalId :: Ident
fractionalId = mkIdent "Fractional"

-- | 'Ident' for the 'Monad' class
monadId :: Ident
monadId = mkIdent "Monad"

-- ---------------------------------------------------------------------------
-- Identifiers for constructors
-- ---------------------------------------------------------------------------

-- | 'Ident' for the value 'True'
trueId :: Ident
trueId = mkIdent "True"

-- | 'Ident' for the value 'False'
falseId :: Ident
falseId = mkIdent "False"

-- | 'Ident' for the value '[]'
nilId :: Ident
nilId = mkIdent "[]"

-- | 'Ident' for the function ':'
consId :: Ident
consId = mkIdent ":"

-- ---------------------------------------------------------------------------
-- Identifiers for values
-- ---------------------------------------------------------------------------

-- | 'Ident' for the main function
mainId :: Ident
mainId = mkIdent "main"

-- | 'Ident' for the minus function
minusId :: Ident
minusId = mkIdent "-"

-- | 'Ident' for the minus function for Floats
fminusId :: Ident
fminusId = mkIdent "-."

-- | 'Ident' for the apply function
applyId :: Ident
applyId = mkIdent "apply"

-- | 'Ident' for the error function
errorId :: Ident
errorId = mkIdent "error"

-- | 'Ident' for the failed function
failedId :: Ident
failedId = mkIdent "failed"

-- | 'Ident' for the id function
idId :: Ident
idId = mkIdent "id"

-- | 'Ident' for the maxBound function
maxBoundId :: Ident
maxBoundId = mkIdent "maxBound"

-- | 'Ident' for the minBound function
minBoundId :: Ident
minBoundId = mkIdent "minBound"

-- | 'Ident' for the pred function
predId :: Ident
predId = mkIdent "pred"

-- | 'Ident' for the succ function
succId :: Ident
succId = mkIdent "succ"

-- | 'Ident' for the toEnum function
toEnumId :: Ident
toEnumId = mkIdent "toEnum"

-- | 'Ident' for the fromEnum function
fromEnumId :: Ident
fromEnumId = mkIdent "fromEnum"

-- | 'Ident' for the enumFrom function
enumFromId :: Ident
enumFromId = mkIdent "enumFrom"

-- | 'Ident' for the enumFromThen function
enumFromThenId :: Ident
enumFromThenId = mkIdent "enumFromThen"

-- | 'Ident' for the enumFromTo function
enumFromToId :: Ident
enumFromToId = mkIdent "enumFromTo"

-- | 'Ident' for the enumFromThenTo function
enumFromThenToId :: Ident
enumFromThenToId = mkIdent "enumFromThenTo"

-- | 'Ident' for the lex function
lexId :: Ident
lexId = mkIdent "lex"

-- | 'Ident' for the readsPrec function
readsPrecId :: Ident
readsPrecId = mkIdent "readsPrec"

-- | 'Ident' for the readParen function
readParenId :: Ident
readParenId = mkIdent "readParen"

-- | 'Ident' for the showsPrec function
showsPrecId :: Ident
showsPrecId = mkIdent "showsPrec"

-- | 'Ident' for the showParen function
showParenId :: Ident
showParenId = mkIdent "showParen"

-- | 'Ident' for the showString function
showStringId :: Ident
showStringId = mkIdent "showString"

-- | 'Ident' for the '&&' operator
andOpId :: Ident
andOpId = mkIdent "&&"

-- | 'Ident' for the '==' operator
eqOpId :: Ident
eqOpId = mkIdent "=="

-- | 'Ident' for the '<=' operator
leqOpId :: Ident
leqOpId = mkIdent "<="

-- | 'Ident' for the '<' operator
ltOpId :: Ident
ltOpId = mkIdent "<"

-- | 'Ident' for the '||' operator
orOpId :: Ident
orOpId = mkIdent "||"

-- | 'Ident' for the '++' operator
appendOpId :: Ident
appendOpId = mkIdent "++"

-- | 'Ident' for the '.' operator
dotOpId :: Ident
dotOpId = mkIdent "."

-- | 'Ident' for anonymous variable
anonId :: Ident
anonId = mkIdent "_"

-- |Check whether an 'Ident' represents an anonymous identifier ('anonId')
isAnonId :: Ident -> Bool
isAnonId = (== anonId) . unRenameIdent

-- ---------------------------------------------------------------------------
-- Qualified Identifiers for types
-- ---------------------------------------------------------------------------

-- | Construct a 'QualIdent' for an 'Ident' using the module prelude
qPreludeIdent :: Ident -> QualIdent
qPreludeIdent = qualifyWith preludeMIdent

-- | 'QualIdent' for the type '(->)'
qArrowId :: QualIdent
qArrowId = qualify arrowId

-- | 'QualIdent' for the type/value unit ('()')
qUnitId :: QualIdent
qUnitId = qualify unitId

-- | 'QualIdent' for the type '[]'
qListId :: QualIdent
qListId = qualify listId

-- | 'QualIdent' for the type 'Bool'
qBoolId :: QualIdent
qBoolId = qPreludeIdent boolId

-- | 'QualIdent' for the type 'Char'
qCharId :: QualIdent
qCharId = qPreludeIdent charId

-- | 'QualIdent' for the type 'Int'
qIntId :: QualIdent
qIntId = qPreludeIdent intId

-- | 'QualIdent' for the type 'Float'
qFloatId :: QualIdent
qFloatId = qPreludeIdent floatId

-- | 'QualIdent' for the type 'IO'
qIOId :: QualIdent
qIOId = qPreludeIdent ioId

-- | 'QualIdent' for the type 'Success'
qSuccessId :: QualIdent
qSuccessId = qPreludeIdent successId

-- | Check whether an 'QualIdent' is an primary type constructor
isPrimTypeId :: QualIdent -> Bool
isPrimTypeId tc = tc `elem` [qArrowId, qUnitId, qListId] || isQTupleId tc

-- ---------------------------------------------------------------------------
-- Qualified Identifiers for type classes
-- ---------------------------------------------------------------------------

-- | 'QualIdent' for the 'Eq' class
qEqId :: QualIdent
qEqId = qPreludeIdent eqId

-- | 'QualIdent' for the 'Ord' class
qOrdId :: QualIdent
qOrdId = qPreludeIdent ordId

-- | 'QualIdent' for the 'Enum' class
qEnumId :: QualIdent
qEnumId = qPreludeIdent enumId

-- | 'QualIdent' for the 'Bounded' class
qBoundedId :: QualIdent
qBoundedId = qPreludeIdent boundedId

-- | 'QualIdent' for the 'Read' class
qReadId :: QualIdent
qReadId = qPreludeIdent readId

-- | 'QualIdent' for the 'Show' class
qShowId :: QualIdent
qShowId = qPreludeIdent showId

-- | 'QualIdent' for the 'Num' class
qNumId :: QualIdent
qNumId = qPreludeIdent numId

-- | 'QualIdent' for the 'Fractional' class
qFractionalId :: QualIdent
qFractionalId = qPreludeIdent fractionalId

-- | 'QualIdent' for the 'Monad' class
qMonadId :: QualIdent
qMonadId = qPreludeIdent monadId

-- ---------------------------------------------------------------------------
-- Qualified Identifiers for constructors
-- ---------------------------------------------------------------------------

-- | 'QualIdent' for the constructor 'True'
qTrueId :: QualIdent
qTrueId = qPreludeIdent trueId

-- | 'QualIdent' for the constructor 'False'
qFalseId :: QualIdent
qFalseId = qPreludeIdent falseId

-- | 'QualIdent' for the constructor '[]'
qNilId :: QualIdent
qNilId = qualify nilId

-- | 'QualIdent' for the constructor ':'
qConsId :: QualIdent
qConsId = qualify consId

-- | 'QualIdent' for the type of n-ary tuples
qTupleId :: Int -> QualIdent
qTupleId = qualify . tupleId

-- | Check whether an 'QualIdent' is an identifier for an tuple type
isQTupleId :: QualIdent -> Bool
isQTupleId = isTupleId . unqualify

-- | Compute the arity of an qualified tuple identifier
qTupleArity :: QualIdent -> Int
qTupleArity = tupleArity . unqualify

-- ---------------------------------------------------------------------------
-- Qualified Identifiers for values
-- ---------------------------------------------------------------------------

-- | 'QualIdent' for the apply function
qApplyId :: QualIdent
qApplyId = qPreludeIdent applyId

-- | 'QualIdent' for the error function
qErrorId :: QualIdent
qErrorId = qPreludeIdent errorId

-- | 'QualIdent' for the failed function
qFailedId :: QualIdent
qFailedId = qPreludeIdent failedId

-- | 'QualIdent' for the id function
qIdId :: QualIdent
qIdId = qPreludeIdent idId

-- | 'QualIdent' for the maxBound function
qMaxBoundId :: QualIdent
qMaxBoundId = qPreludeIdent maxBoundId

-- | 'QualIdent' for the minBound function
qMinBoundId :: QualIdent
qMinBoundId = qPreludeIdent minBoundId

-- | 'QualIdent' for the fromEnum function
qFromEnumId :: QualIdent
qFromEnumId = qPreludeIdent fromEnumId

-- | 'QualIdent' for the enumFrom function
qEnumFromId :: QualIdent
qEnumFromId = qPreludeIdent enumFromId

-- | 'QualIdent' for the enumFromThen function
qEnumFromThenId :: QualIdent
qEnumFromThenId = qPreludeIdent enumFromThenId

-- | 'QualIdent' for the enumFromTo function
qEnumFromToId :: QualIdent
qEnumFromToId = qPreludeIdent enumFromToId

-- | 'QualIdent' for the enumFromThenTo function
qEnumFromThenToId :: QualIdent
qEnumFromThenToId = qPreludeIdent enumFromThenToId

-- | 'QualIdent' for the lex function
qLexId :: QualIdent
qLexId = qPreludeIdent lexId

-- | 'QualIdent' for the readsPrec function
qReadsPrecId :: QualIdent
qReadsPrecId = qPreludeIdent readsPrecId

-- | 'QualIdent' for the readParen function
qReadParenId :: QualIdent
qReadParenId = qPreludeIdent readParenId

-- | 'QualIdent' for the showsPrec function
qShowsPrecId :: QualIdent
qShowsPrecId = qPreludeIdent showsPrecId

-- | 'QualIdent' for the showParen function
qShowParenId :: QualIdent
qShowParenId = qPreludeIdent showParenId

-- | 'QualIdent' for the showString function
qShowStringId :: QualIdent
qShowStringId = qPreludeIdent showStringId

-- | 'QualIdent' for the '&&' operator
qAndOpId :: QualIdent
qAndOpId = qPreludeIdent andOpId

-- | 'QualIdent' for the '==' operator
qEqOpId :: QualIdent
qEqOpId = qPreludeIdent eqOpId

-- | 'QualIdent' for the '<=' operator
qLeqOpId :: QualIdent
qLeqOpId = qPreludeIdent leqOpId

-- | 'QualIdent' for the '<' operator
qLtOpId :: QualIdent
qLtOpId = qPreludeIdent ltOpId

-- | 'QualIdent' for the '||' operator
qOrOpId :: QualIdent
qOrOpId = qPreludeIdent orOpId

-- | 'QualIdent' for the '.' operator
qDotOpId :: QualIdent
qDotOpId = qPreludeIdent dotOpId

-- | 'QualIdent' for the '++' operator
qAppendOpId :: QualIdent
qAppendOpId = qPreludeIdent appendOpId
