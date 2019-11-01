-- |
-- Description: AST for curry code
-- Author     : Kai-Oliver Prott
-- Version    : August 2018
--
-- An implementation of the Curry AST from curry-frontend
module Curry.Types where

import Curry.SpanInfo
import Curry.Ident
import Curry.Position

-- | This datatype is copied from curry-base.
data Module a = Module SpanInfo [ModulePragma] ModuleIdent
                       (Maybe ExportSpec) [ImportDecl] [Decl a]
    deriving (Eq, Read, Show)

data ModulePragma
  = LanguagePragma SpanInfo [Extension]
  | OptionsPragma  SpanInfo (Maybe Tool) String
    deriving (Eq, Read, Show)

data ExportSpec = Exporting SpanInfo [Export]
    deriving (Eq, Read, Show)

data Export
  = Export         SpanInfo QualIdent
  | ExportTypeWith SpanInfo QualIdent [Ident]
  | ExportTypeAll  SpanInfo QualIdent
  | ExportModule   SpanInfo ModuleIdent
    deriving (Eq, Read, Show)

data ImportDecl = ImportDecl SpanInfo ModuleIdent Qualified
                             (Maybe ModuleIdent) (Maybe ImportSpec)
    deriving (Eq, Read, Show)

type Qualified = Bool

data ImportSpec
  = Importing SpanInfo [Import]
  | Hiding    SpanInfo [Import]
    deriving (Eq, Read, Show)

data Import
  = Import         SpanInfo Ident
  | ImportTypeWith SpanInfo Ident [Ident]
  | ImportTypeAll  SpanInfo Ident
    deriving (Eq, Read, Show)

data Decl a
  = InfixDecl        SpanInfo Infix (Maybe Precedence) [Ident]         -- infixl 5 (op), `fun`
  | DataDecl         SpanInfo Ident [Ident] [ConstrDecl] [QualIdent]   -- data C a b = C1 a | C2 b deriving (D, ...)
  | ExternalDataDecl SpanInfo Ident [Ident]
  | NewtypeDecl      SpanInfo Ident [Ident] NewConstrDecl [QualIdent]  -- newtype C a b = C a b deriving (D, ...)
  | TypeDecl         SpanInfo Ident [Ident] TypeExpr                   -- type C a b = D a b
  | TypeSig          SpanInfo [Ident] QualTypeExpr                     -- f, g :: Bool
  | FunctionDecl     SpanInfo a Ident [Equation a]                     -- f True = 1 ; f False = 0
  | ExternalDecl     SpanInfo [Var a]                                  -- f, g external
  | PatternDecl      SpanInfo (Pattern a) (Rhs a)                      -- Just x = ...
  | FreeDecl         SpanInfo [Var a]                                  -- x, y free
  | DefaultDecl      SpanInfo [TypeExpr]                               -- default (Int, Float)
  | ClassDecl        SpanInfo Context Ident Ident [Decl a]             -- class C a => D a where {TypeSig|InfixDecl|FunctionDecl}
  | InstanceDecl     SpanInfo Context QualIdent InstanceType [Decl a]  -- instance C a => M.D (N.T a b c) where {FunctionDecl}
  deriving (Eq, Read, Show)

type Precedence = Int

data Infix
  = InfixL
  | InfixR
  | Infix
    deriving (Eq, Read, Show)

data ConstrDecl
  = ConstrDecl SpanInfo Ident [TypeExpr]
  | ConOpDecl  SpanInfo TypeExpr Ident TypeExpr
  | RecordDecl SpanInfo Ident [FieldDecl]
    deriving (Eq, Read, Show)

data NewConstrDecl
  = NewConstrDecl SpanInfo Ident TypeExpr
  | NewRecordDecl SpanInfo Ident (Ident, TypeExpr)
   deriving (Eq, Read, Show)

data FieldDecl = FieldDecl SpanInfo [Ident] TypeExpr
  deriving (Eq, Read, Show)

data TypeExpr
  = ConstructorType SpanInfo QualIdent
  | ApplyType       SpanInfo TypeExpr TypeExpr
  | VariableType    SpanInfo Ident
  | TupleType       SpanInfo [TypeExpr]
  | ListType        SpanInfo TypeExpr
  | ArrowType       SpanInfo TypeExpr TypeExpr
  | ParenType       SpanInfo TypeExpr
  | ForallType      SpanInfo [Ident] TypeExpr
  deriving (Eq, Read, Show)

data QualTypeExpr = QualTypeExpr SpanInfo Context TypeExpr
    deriving (Eq, Read, Show)

type Context = [Constraint]

data Constraint = Constraint SpanInfo QualIdent TypeExpr
  deriving (Eq, Read, Show)

type InstanceType = TypeExpr

data Equation a = Equation SpanInfo (Lhs a) (Rhs a)
  deriving (Eq, Read, Show)

data Lhs a
  = FunLhs SpanInfo Ident [Pattern a]
  | OpLhs  SpanInfo (Pattern a) Ident (Pattern a)
  | ApLhs  SpanInfo (Lhs a) [Pattern a]
  deriving (Eq, Read, Show)

data Rhs a
  = SimpleRhs  SpanInfo (Expression a) [Decl a]
  | GuardedRhs SpanInfo [CondExpr a] [Decl a]
  deriving (Eq, Read, Show)

data CondExpr a = CondExpr SpanInfo (Expression a) (Expression a)
  deriving (Eq, Read, Show)

data Literal
  = Char   Char
  | Int    Int
  | Float  Float
  | String String
  deriving (Eq, Read, Show)

data Pattern a
  = LiteralPattern     SpanInfo a Literal
  | NegativePattern    SpanInfo a Literal
  | VariablePattern    SpanInfo a Ident
  | ConstructorPattern SpanInfo a QualIdent [Pattern a]
  | InfixPattern       SpanInfo a (Pattern a) QualIdent (Pattern a)
  | ParenPattern       SpanInfo (Pattern a)
  | RecordPattern      SpanInfo a QualIdent [Field (Pattern a)]
  | TuplePattern       SpanInfo [Pattern a]
  | ListPattern        SpanInfo a [Pattern a]
  | AsPattern          SpanInfo Ident (Pattern a)
  | LazyPattern        SpanInfo (Pattern a)
  | FunctionPattern    SpanInfo a QualIdent [Pattern a]
  | InfixFuncPattern   SpanInfo a (Pattern a) QualIdent (Pattern a)
  deriving (Eq, Read, Show)

data Expression a
  = Literal           SpanInfo a Literal
  | Variable          SpanInfo a QualIdent
  | Constructor       SpanInfo a QualIdent
  | Paren             SpanInfo (Expression a)
  | Typed             SpanInfo (Expression a) QualTypeExpr
  | Record            SpanInfo a QualIdent [Field (Expression a)]    -- C {l1 = e1,..., ln = en}
  | RecordUpdate      SpanInfo (Expression a) [Field (Expression a)] -- e {l1 = e1,..., ln = en}
  | Tuple             SpanInfo [Expression a]
  | List              SpanInfo a [Expression a]
  | ListCompr         SpanInfo (Expression a) [Statement a]   -- the ref corresponds to the main list
  | EnumFrom          SpanInfo (Expression a)
  | EnumFromThen      SpanInfo (Expression a) (Expression a)
  | EnumFromTo        SpanInfo (Expression a) (Expression a)
  | EnumFromThenTo    SpanInfo (Expression a) (Expression a) (Expression a)
  | UnaryMinus        SpanInfo (Expression a)
  | Apply             SpanInfo (Expression a) (Expression a)
  | InfixApply        SpanInfo (Expression a) (InfixOp a) (Expression a)
  | LeftSection       SpanInfo (Expression a) (InfixOp a)
  | RightSection      SpanInfo (InfixOp a) (Expression a)
  | Lambda            SpanInfo [Pattern a] (Expression a)
  | Let               SpanInfo [Decl a] (Expression a)
  | Do                SpanInfo [Statement a] (Expression a)
  | IfThenElse        SpanInfo (Expression a) (Expression a) (Expression a)
  | Case              SpanInfo CaseType (Expression a) [Alt a]
  deriving (Eq, Read, Show)

data InfixOp a
  = InfixOp     a QualIdent
  | InfixConstr a QualIdent
  deriving (Eq, Read, Show)

data Statement a
  = StmtExpr SpanInfo (Expression a)
  | StmtDecl SpanInfo [Decl a]
  | StmtBind SpanInfo (Pattern a) (Expression a)
  deriving (Eq, Read, Show)

data CaseType
  = Rigid
  | Flex
  deriving (Eq, Read, Show)

data Alt a = Alt SpanInfo (Pattern a) (Rhs a)
  deriving (Eq, Read, Show)

data Field a = Field SpanInfo QualIdent a
  deriving (Eq, Read, Show)

data Var a = Var a Ident
  deriving (Eq, Read, Show)

data Extension
  = KnownExtension   Position KnownExtension
  | UnknownExtension Position String
  deriving (Eq, Read, Show)

data KnownExtension
  = AnonFreeVars
  | CPP
  | ExistentialQuantification
  | FunctionalPatterns
  | NegativeLiterals
  | NoImplicitPrelude
  deriving (Eq, Read, Show)

data Tool = KICS2 | PAKCS | CYMAKE | FRONTEND | UnknownTool String
  deriving (Eq, Read, Show)

instance HasSpanInfo (Module a) where
  getSpanInfo (Module sp _ _ _ _ _) = sp
  setSpanInfo sp (Module _ ps m es is ds) = Module sp ps m es is ds

instance HasSpanInfo (Decl a) where
  getSpanInfo (InfixDecl        sp _ _ _)   = sp
  getSpanInfo (DataDecl         sp _ _ _ _) = sp
  getSpanInfo (ExternalDataDecl sp _ _)     = sp
  getSpanInfo (NewtypeDecl      sp _ _ _ _) = sp
  getSpanInfo (TypeDecl         sp _ _ _)   = sp
  getSpanInfo (TypeSig          sp _ _)     = sp
  getSpanInfo (FunctionDecl     sp _ _ _)   = sp
  getSpanInfo (ExternalDecl     sp _)       = sp
  getSpanInfo (PatternDecl      sp _ _)     = sp
  getSpanInfo (FreeDecl         sp _)       = sp
  getSpanInfo (DefaultDecl      sp _)       = sp
  getSpanInfo (ClassDecl        sp _ _ _ _) = sp
  getSpanInfo (InstanceDecl     sp _ _ _ _) = sp

  setSpanInfo sp (InfixDecl _ fix prec ops) = InfixDecl sp fix prec ops
  setSpanInfo sp (DataDecl _ tc tvs cs clss) = DataDecl sp tc tvs cs clss
  setSpanInfo sp (ExternalDataDecl _ tc tvs) = ExternalDataDecl sp tc tvs
  setSpanInfo sp (NewtypeDecl _ tc tvs nc clss) = NewtypeDecl sp tc tvs nc clss
  setSpanInfo sp (TypeDecl _ tc tvs ty) = TypeDecl sp tc tvs ty
  setSpanInfo sp (TypeSig _ fs qty) = TypeSig sp fs qty
  setSpanInfo sp (FunctionDecl _ a f' eqs) = FunctionDecl sp a f' eqs
  setSpanInfo sp (ExternalDecl _ vs) = ExternalDecl sp vs
  setSpanInfo sp (PatternDecl _ t rhs) = PatternDecl sp t rhs
  setSpanInfo sp (FreeDecl _ vs) = FreeDecl sp vs
  setSpanInfo sp (DefaultDecl _ tys) = DefaultDecl sp tys
  setSpanInfo sp (ClassDecl _ cx cls clsvar ds) = ClassDecl sp cx cls clsvar ds
  setSpanInfo sp (InstanceDecl _ cx qcls inst ds) = InstanceDecl sp cx qcls inst ds

instance HasSpanInfo (Equation a) where
  getSpanInfo (Equation spi _ _) = spi
  setSpanInfo spi (Equation _ lhs rhs) = Equation spi lhs rhs

instance HasSpanInfo ModulePragma where
  getSpanInfo (LanguagePragma sp _  ) = sp
  getSpanInfo (OptionsPragma  sp _ _) = sp

  setSpanInfo sp (LanguagePragma _ ex ) = LanguagePragma sp ex
  setSpanInfo sp (OptionsPragma  _ t a) = OptionsPragma sp t a

instance HasSpanInfo ExportSpec where
  getSpanInfo (Exporting sp _) = sp
  setSpanInfo sp (Exporting _ ex) = Exporting sp ex

instance HasSpanInfo Export where
  getSpanInfo (Export sp _)           = sp
  getSpanInfo (ExportTypeWith sp _ _) = sp
  getSpanInfo (ExportTypeAll sp _)    = sp
  getSpanInfo (ExportModule sp _)     = sp

  setSpanInfo sp (Export _ qid)            = Export sp qid
  setSpanInfo sp (ExportTypeWith _ qid cs) = ExportTypeWith sp qid cs
  setSpanInfo sp (ExportTypeAll _ qid)     = ExportTypeAll sp qid
  setSpanInfo sp (ExportModule _ mid)      = ExportModule sp mid

instance HasSpanInfo ImportDecl where
  getSpanInfo (ImportDecl sp _ _ _ _) = sp
  setSpanInfo sp (ImportDecl _ mid q as spec) = ImportDecl sp mid q as spec

instance HasSpanInfo ImportSpec where
  getSpanInfo (Importing sp _) = sp
  getSpanInfo (Hiding    sp _) = sp

  setSpanInfo sp (Importing _ im) = Importing sp im
  setSpanInfo sp (Hiding    _ im) = Hiding sp im

instance HasSpanInfo Import where
  getSpanInfo (Import sp _)           = sp
  getSpanInfo (ImportTypeWith sp _ _) = sp
  getSpanInfo (ImportTypeAll sp _)    = sp

  setSpanInfo sp (Import _ qid)            = Import sp qid
  setSpanInfo sp (ImportTypeWith _ qid cs) = ImportTypeWith sp qid cs
  setSpanInfo sp (ImportTypeAll _ qid)     = ImportTypeAll sp qid

instance HasSpanInfo ConstrDecl where
  getSpanInfo (ConstrDecl sp _ _)   = sp
  getSpanInfo (ConOpDecl  sp _ _ _) = sp
  getSpanInfo (RecordDecl sp _ _)   = sp

  setSpanInfo sp (ConstrDecl _ idt ty) = ConstrDecl sp idt ty
  setSpanInfo sp (ConOpDecl  _ ty1 idt ty2) = ConOpDecl sp ty1 idt ty2
  setSpanInfo sp (RecordDecl _ idt fd) = RecordDecl sp idt fd

instance HasSpanInfo NewConstrDecl where
  getSpanInfo (NewConstrDecl sp _ _)   = sp
  getSpanInfo (NewRecordDecl sp _ _)   = sp

  setSpanInfo sp (NewConstrDecl _ idt ty)  = NewConstrDecl sp idt ty
  setSpanInfo sp (NewRecordDecl _ idt fty) = NewRecordDecl sp idt fty

instance HasSpanInfo FieldDecl where
    getSpanInfo (FieldDecl sp _ _) = sp
    setSpanInfo sp (FieldDecl _ idt ty) = FieldDecl sp idt ty

instance HasSpanInfo TypeExpr where
  getSpanInfo (ConstructorType sp _) = sp
  getSpanInfo (ApplyType sp _ _)     = sp
  getSpanInfo (VariableType sp _)    = sp
  getSpanInfo (TupleType sp _)       = sp
  getSpanInfo (ListType sp _)        = sp
  getSpanInfo (ArrowType sp _ _)     = sp
  getSpanInfo (ParenType sp _)       = sp
  getSpanInfo (ForallType sp _ _)    = sp

  setSpanInfo sp (ConstructorType _ qid) = ConstructorType sp qid
  setSpanInfo sp (ApplyType _ ty1 ty2)   = ApplyType sp ty1 ty2
  setSpanInfo sp (VariableType _ idt)    = VariableType sp idt
  setSpanInfo sp (TupleType _ tys)       = TupleType sp tys
  setSpanInfo sp (ListType _ ty)         = ListType sp ty
  setSpanInfo sp (ArrowType _ ty1 ty2)   = ArrowType sp ty1 ty2
  setSpanInfo sp (ParenType _ ty)        = ParenType sp ty
  setSpanInfo sp (ForallType _ idt ty)   = ForallType sp idt ty

instance HasSpanInfo QualTypeExpr where
  getSpanInfo (QualTypeExpr sp _ _) = sp
  setSpanInfo sp (QualTypeExpr _ cx ty) = QualTypeExpr sp cx ty

instance HasSpanInfo Constraint where
  getSpanInfo (Constraint sp _ _) = sp
  setSpanInfo sp (Constraint _ qid ty) = Constraint sp qid ty

instance HasSpanInfo (Lhs a) where
  getSpanInfo (FunLhs sp _ _)   = sp
  getSpanInfo (OpLhs  sp _ _ _) = sp
  getSpanInfo (ApLhs  sp _ _)   = sp

  setSpanInfo sp (FunLhs _ idt ps)    = FunLhs sp idt ps
  setSpanInfo sp (OpLhs  _ p1 idt p2) = OpLhs sp p1 idt p2
  setSpanInfo sp (ApLhs  _ lhs ps)    = ApLhs sp lhs ps

instance HasSpanInfo (Rhs a) where
  getSpanInfo (SimpleRhs sp _ _)  = sp
  getSpanInfo (GuardedRhs sp _ _) = sp

  setSpanInfo sp (SimpleRhs _ ex ds)  = SimpleRhs sp ex ds
  setSpanInfo sp (GuardedRhs _ cs ds) = GuardedRhs sp cs ds

instance HasSpanInfo (CondExpr a) where
    getSpanInfo (CondExpr sp _ _) = sp
    setSpanInfo sp (CondExpr _ e1 e2) = CondExpr sp e1 e2

instance HasSpanInfo (Pattern a) where
  getSpanInfo (LiteralPattern  sp _ _)      = sp
  getSpanInfo (NegativePattern sp _ _)      = sp
  getSpanInfo (VariablePattern sp _ _)      = sp
  getSpanInfo (ConstructorPattern sp _ _ _) = sp
  getSpanInfo (InfixPattern sp _ _ _ _)     = sp
  getSpanInfo (ParenPattern sp _)           = sp
  getSpanInfo (RecordPattern sp _ _ _)      = sp
  getSpanInfo (TuplePattern sp _)           = sp
  getSpanInfo (ListPattern sp _ _)          = sp
  getSpanInfo (AsPattern sp _ _)            = sp
  getSpanInfo (LazyPattern sp _)            = sp
  getSpanInfo (FunctionPattern sp _ _ _)    = sp
  getSpanInfo (InfixFuncPattern sp _ _ _ _) = sp

  setSpanInfo sp (LiteralPattern _ a l) = LiteralPattern sp a l
  setSpanInfo sp (NegativePattern _ a l) = NegativePattern sp a l
  setSpanInfo sp (VariablePattern _ a v) = VariablePattern sp a v
  setSpanInfo sp (ConstructorPattern _ a c ts) = ConstructorPattern sp a c ts
  setSpanInfo sp (InfixPattern _ a t1 op t2) = InfixPattern sp a t1 op t2
  setSpanInfo sp (ParenPattern _ t) = ParenPattern sp t
  setSpanInfo sp (RecordPattern _ a c fs) = RecordPattern sp a c fs
  setSpanInfo sp (TuplePattern _ ts) = TuplePattern sp ts
  setSpanInfo sp (ListPattern _ a ts) = ListPattern sp a ts
  setSpanInfo sp (AsPattern _ v t) = AsPattern sp v t
  setSpanInfo sp (LazyPattern _ t) = LazyPattern sp t
  setSpanInfo sp (FunctionPattern _ a f' ts) = FunctionPattern sp a f' ts
  setSpanInfo sp (InfixFuncPattern _ a t1 op t2) = InfixFuncPattern sp a t1 op t2

instance HasSpanInfo (Expression a) where
  getSpanInfo (Literal sp _ _) = sp
  getSpanInfo (Variable sp _ _) = sp
  getSpanInfo (Constructor sp _ _) = sp
  getSpanInfo (Paren sp _) = sp
  getSpanInfo (Typed sp _ _) = sp
  getSpanInfo (Record sp _ _ _) = sp
  getSpanInfo (RecordUpdate sp _ _) = sp
  getSpanInfo (Tuple sp _) = sp
  getSpanInfo (List sp _ _) = sp
  getSpanInfo (ListCompr sp _ _) = sp
  getSpanInfo (EnumFrom sp _) = sp
  getSpanInfo (EnumFromThen sp _ _) = sp
  getSpanInfo (EnumFromTo sp _ _) = sp
  getSpanInfo (EnumFromThenTo sp _ _ _) = sp
  getSpanInfo (UnaryMinus sp _) = sp
  getSpanInfo (Apply sp _ _) = sp
  getSpanInfo (InfixApply sp _ _ _) = sp
  getSpanInfo (LeftSection sp _ _) = sp
  getSpanInfo (RightSection sp _ _) = sp
  getSpanInfo (Lambda sp _ _) = sp
  getSpanInfo (Let sp _ _) = sp
  getSpanInfo (Do sp _ _) = sp
  getSpanInfo (IfThenElse sp _ _ _) = sp
  getSpanInfo (Case sp _ _ _) = sp

  setSpanInfo sp (Literal _ a l) = Literal sp a l
  setSpanInfo sp (Variable _ a v) = Variable sp a v
  setSpanInfo sp (Constructor _ a c) = Constructor sp a c
  setSpanInfo sp (Paren _ e) = Paren sp e
  setSpanInfo sp (Typed _ e qty) = Typed sp e qty
  setSpanInfo sp (Record _ a c fs) = Record sp a c fs
  setSpanInfo sp (RecordUpdate _ e fs) = RecordUpdate sp e fs
  setSpanInfo sp (Tuple _ es) = Tuple sp es
  setSpanInfo sp (List _ a es) = List sp a es
  setSpanInfo sp (ListCompr _ e stms) = ListCompr sp e stms
  setSpanInfo sp (EnumFrom _ e) = EnumFrom sp e
  setSpanInfo sp (EnumFromThen _ e1 e2) = EnumFromThen sp e1 e2
  setSpanInfo sp (EnumFromTo _ e1 e2) = EnumFromTo sp e1 e2
  setSpanInfo sp (EnumFromThenTo _ e1 e2 e3) = EnumFromThenTo sp e1 e2 e3
  setSpanInfo sp (UnaryMinus _ e) = UnaryMinus sp e
  setSpanInfo sp (Apply _ e1 e2) = Apply sp e1 e2
  setSpanInfo sp (InfixApply _ e1 op e2) = InfixApply sp e1 op e2
  setSpanInfo sp (LeftSection _ e op) = LeftSection sp e op
  setSpanInfo sp (RightSection _ op e) = RightSection sp op e
  setSpanInfo sp (Lambda _ ts e) = Lambda sp ts e
  setSpanInfo sp (Let _ ds e) = Let sp ds e
  setSpanInfo sp (Do _ stms e) = Do sp stms e
  setSpanInfo sp (IfThenElse _ e1 e2 e3) = IfThenElse sp e1 e2 e3
  setSpanInfo sp (Case _ ct e as) = Case sp ct e as

instance HasSpanInfo (Statement a) where
  getSpanInfo (StmtExpr sp _)   = sp
  getSpanInfo (StmtDecl sp _)   = sp
  getSpanInfo (StmtBind sp _ _) = sp

  setSpanInfo sp (StmtExpr _ ex)   = StmtExpr sp ex
  setSpanInfo sp (StmtDecl _ ds)   = StmtDecl sp ds
  setSpanInfo sp (StmtBind _ p ex) = StmtBind sp p ex

instance HasSpanInfo (Alt a) where
  getSpanInfo (Alt sp _ _) = sp
  setSpanInfo sp (Alt _ p rhs) = Alt sp p rhs

instance HasSpanInfo (Field a) where
    getSpanInfo (Field sp _ _) = sp
    setSpanInfo sp (Field _ qid a) = Field sp qid a
