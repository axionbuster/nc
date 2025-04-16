{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : NC.Type.Def
-- Description : Core type definitions for the C language
-- License     : BSD-3-Clause
-- Maintainer  : axionbuster@gmail.com
--
-- This module defines all the core data types for representing C language
-- constructs including types, expressions, statements, declarations, and
-- literals. It provides a complete AST (Abstract Syntax Tree) representation
-- for parsing and manipulating C code.
module NC.Type.Def (
  -- * Symbols and names
  Name,
  Symbol,
  Table,

  -- * Types and Symbols

  -- ** C Type System
  Type (..),
  ty_base,
  ty_qual,
  ty_align,
  UQType (..),
  Qual,
  _qu_none,
  _qu_const,
  _qu_restrict,
  _qu_volatile,
  _qu_atomic,
  qu_none,
  qu_any,
  qu_const,
  qu_restrict,
  qu_volatile,
  qu_atomic,
  Alignment (..),

  -- ** Record Types
  RecInfo (..),
  rec_type,
  rec_sym,
  rec_attrs,
  rec_def,
  RecType (..),
  RecMember (..),

  -- ** Array Types
  ArrayInfo (..),
  arr_size,
  arr_type,
  arr_paraminfo,
  ParamArrayInfo (..),
  parr_static,
  parr_qual,
  ArrayStatic (..),

  -- ** Enum Types
  EnumInfo (..),
  enum_sym,
  enum_attrs,
  enum_membertype,
  enum_members,
  EnumConst (..),
  ec_sym,
  ec_attrs,
  ec_explicitval,

  -- ** Function Types
  FuncInfo (..),
  fun_rettype,
  fun_pars,
  fun_variadic,
  Param (..),
  Variadic (..),

  -- ** Metaprogramming Types
  Typeof (..),
  TypeofQual (..),

  -- ** C Attributes
  Attribute (..),
  attr_prefix,
  attr_basename,
  attr_info,

  -- * C Declarations
  Declaration (..),
  DeclInit (..),
  decl_type,
  decl_stor,
  decl_init,
  Declarator (..),
  StaticAssertion (..),
  sa_expr,
  sa_mesg,
  StorageClass (..),

  -- * C Expressions

  -- ** Expression Types
  Expr (..),
  PrimExpr (..),
  UnaryOp (..),
  BinOp (..),
  SpecialExpr (..),
  GenAssoc (..),
  ConstIntExpr (..),
  mkconstintexpr,

  -- ** Expression Pattern Synonyms
  pattern ExprPostInc,
  pattern ExprPostDec,
  pattern ExprPreInc,
  pattern ExprPreDec,
  pattern ExprUnaryPlus,
  pattern ExprUnaryMinus,
  pattern ExprLogNot,
  pattern ExprBitNot,
  pattern ExprDeref,
  pattern ExprAddrOf,
  pattern ExprIndex,
  pattern ExprTimes,
  pattern ExprDiv,
  pattern ExprMod,
  pattern ExprPlus,
  pattern ExprMinus,
  pattern ExprShiftL,
  pattern ExprShiftR,
  pattern ExprLT,
  pattern ExprGT,
  pattern ExprLE,
  pattern ExprGE,
  pattern ExprEQ,
  pattern ExprNE,
  pattern ExprBitAnd,
  pattern ExprBitXor,
  pattern ExprBitOr,
  pattern ExprLogAnd,
  pattern ExprLogOr,
  pattern ExprAssign,
  pattern ExprAssignPlus,
  pattern ExprAssignMinus,
  pattern ExprAssignTimes,
  pattern ExprAssignDiv,
  pattern ExprAssignShiftL,
  pattern ExprAssignShiftR,
  pattern ExprAssignBitAnd,
  pattern ExprAssignBitOr,
  pattern ExprSequence,
  pattern ExprCall,
  pattern ExprMember,
  pattern ExprMemberPtr,
  pattern ExprCompoundLiteral,
  pattern ExprSizeof,
  pattern ExprAlignof,
  pattern ExprITE,
  pattern ExprId,
  pattern ExprLit,
  pattern ExprParen,
  pattern ExprGeneric,

  -- * C Statements
  Statement,
  StmtBody (..),
  BlockItem (..),
  Label (..),
  lab_attrs,
  lab_sym,
  ForInit (..),
  Jump (..),

  -- * C Initializers
  Initializer (..),
  InitItem (..),
  init_designation,
  init_value,
  Designator (..),

  -- * C Literals
  Lit (..),
  IntegerLiteral (..),
  CharacterLiteral (..),
  StringLiteral (..),
  lit_prim,
) where

import Control.Lens
import Control.Monad.IO.Class
import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (LazyByteString)
import Data.Coerce
import Data.Function
import Data.HashTable.IO qualified as H
import Data.Hashable
import Data.Semigroup
import Data.Sequence (Seq)
import Data.Unique
import {-# SOURCE #-} NC.Parser.Def
import NC.Type.Prim
import UnliftIO.IORef
import Prelude

-- * Type definitions

-- ** Symbols and tables

-- | Due to "FlatParse"'s design, 'ByteString' excerpts share the same
-- underlying buffer.
type Name = ByteString

-- | An opaque thing that identifies an object.
newtype Symbol = Symbol Unique
  deriving newtype (Eq)

-- | We use an IO-based hash table.
type Table k v = H.BasicHashTable k v

-- ** C Types (organization)

-- | Qualified type
data Type
  = Type
  { _ty_base :: UQType,
    _ty_qual :: Qual,
    _ty_align :: Alignment
  }
  deriving (Show, Eq)

-- | Unqualified type
data UQType
  = UQPrim Prim
  | UQFunc FuncInfo
  | UQPointer Type
  | UQArray ArrayInfo
  | UQRecord RecInfo
  | UQEnum EnumInfo
  | UQAtomic UQType
  | UQTypeof TypeofQual Typeof
  deriving (Show, Eq)

-- | A C attribute
data Attribute = Attribute
  { _attr_prefix :: Maybe Name,
    _attr_basename :: Name,
    _attr_info :: Maybe Span64
  }
  deriving (Show, Eq)

-- | A C qualification
newtype Qual = Qual Word16
  deriving (Show, Eq, Bits)
  deriving (Semigroup, Monoid) via (Ior Qual)

-- | A C alignment specification
data Alignment
  = AlignNone
  | AlignAs ConstIntExpr
  | AlignAsType Type
  deriving (Show, Eq)

-- *** C Record types (struct, union)

-- | Information about a C record type
data RecInfo = RecInfo
  { _rec_type :: RecType,
    _rec_sym :: Symbol,
    _rec_attrs :: [Attribute],
    _rec_def :: Maybe [RecMember]
  }
  deriving (Show, Eq)

-- | Is the record a @struct@ or a @union@?
data RecType = RecStruct | RecUnion
  deriving (Show, Eq)

-- | A C record member may be a field, which can be a bitfield, or
-- it may be a @static\_assert@.
data RecMember
  = RMField [Attribute] Type Symbol (Maybe ConstIntExpr)
  | RMStaticAssertion StaticAssertion
  deriving (Show, Eq)

-- *** C Array

-- | A C array
data ArrayInfo = ArrayInfo
  { -- | A C array size may be a VLA or given by a constant length.
    _arr_size :: Expr,
    -- | Element type.
    _arr_type :: Type,
    -- | If this array derivation is the outermost derivation of an array
    -- that is a function parameter, this information may exist. Otherwise,
    -- it's illegal for this to exist.
    _arr_paraminfo :: Maybe ParamArrayInfo
  }
  deriving (Show, Eq)

-- | The outermost derivation of an array in parameter position may be
-- given extra annotations.
data ParamArrayInfo = ParamArrayInfo
  { -- | If set, there will be at least a given number of elements. This
    -- implies that the array will exist.
    _parr_static :: ArrayStatic,
    -- | Qualifications for the decayed pointer.
    _parr_qual :: Qual
  }
  deriving (Show, Eq)

-- | In parameter position, and in the outermost derivation, an array can be
-- given a @static@ annotation. @static@ means that the array exists
-- (is not null), and there will be /at least/ a given number of elements.
data ArrayStatic
  = ASNoStatic
  | ASStatic
  deriving (Show, Eq)

-- *** C Enumeration types

-- | A C @enum@ type
data EnumInfo = EnumInfo
  { _enum_sym :: Symbol,
    _enum_attrs :: [Attribute],
    _enum_membertype :: Type,
    _enum_members :: Maybe [EnumConst]
  }
  deriving (Show, Eq)

-- | A constant declared in a C @enum@
data EnumConst = EnumConst
  { _ec_sym :: Symbol,
    _ec_attrs :: [Attribute],
    _ec_explicitval :: Maybe ConstIntExpr
  }
  deriving (Show, Eq)

-- *** C Functions

-- | C function type
data FuncInfo = FuncInfo
  { _fun_rettype :: Type,
    _fun_pars :: [Param],
    _fun_variadic :: Variadic
  }
  deriving (Show, Eq)

-- | A function parameter that optionally introduces an identifier into
-- scope.
data Param = Param Type (Maybe Symbol)
  deriving (Show, Eq)

-- | Is a function variadic?
data Variadic = Variadic | NotVariadic
  deriving (Show, Eq)

-- *** C Metaprogramming types

-- | Argument of @typeof@ or @typeof\_unqual@
data Typeof = TypeofType Type | TypeofExpr Expr
  deriving (Show, Eq)

-- | Is it a @typeof@ or @typeof\_unqual@?
data TypeofQual = TQQual | TQUnqual
  deriving (Show, Eq)

-- ** C Declarations, expressions, statements, and initializers.

-- *** C declarations

-- | A C declarator transforms a base type into a more complex derived type.
--
-- In C declarations, a declarator defines how a base type is modified to create
-- the final type of an identifier. For example, given a declaration like:
--
-- @
-- static long _Complex double a, *b(void), (*c)[2];
-- @
--
-- The base type is @static long _Complex double@, and there are three
-- declarators:
--
-- - @a@ (simple identifier - no transformation)
-- - @*b(void)@ (function returning pointer)
-- - @(*c)[2]@ (pointer to array of 2 elements)
--
-- Declarators come in two forms:
--
-- 1. Regular declarators introduce an identifier (like @a@, @b@, @c@ above)
-- 2. Abstract declarators have no identifier (e.g. @(*)(int, long)@)
--
-- A declarator is represented as a function that transforms the base type into
-- the final derived type. For example, to determine the type of @b@ above, we
-- apply a transformation that turns the base type into a function returning a
-- pointer to the base type.
--
-- == 'Semigroup' and 'Monoid' instances
--
-- The 'Semigroup' and 'Monoid' instances for 'Declarator' are designed to
-- compose type transformations in the reverse order from how they are written
-- in the source code. This is necessary because C declarators are read from the
-- inside out. For example:
--
-- @
-- int *(*foo[10])(void)
-- @
--
-- Here, the transformations are applied in reverse of their syntactic
-- appearance:
--
-- 1. @foo@ is an identifier (creates a variable)
-- 2. @[10]@ makes it an array of 10 elements
-- 3. @*@ makes each element a pointer
-- 4. @(void)@ makes that pointer a function (taking no args)
-- 5. @*@ makes the return value of that function a pointer
-- 6. @int@ is the base type that this pointer points to
--
-- The `Dual (Endo Type)` deriving mechanism ensures that when declarators are
-- combined, the rightmost transformation is applied first (inside-out), which
-- matches C's declarator evaluation rules.
newtype Declarator = Declarator {apdecl :: Type -> Type}
  deriving (Monoid, Semigroup) via (Dual (Endo Type))

-- | A C declaration.
data Declaration
  = -- | The most normal type of declaration. It introduces a series
    -- of declarators.
    ListDecl StorageClass [DeclInit]
  | -- | A C function definition.
    FuncDef StorageClass FuncInfo Statement
  | -- | A C @static\_assertion@.
    StaticAssertDecl StaticAssertion
  | -- | A standalone attribute declaration.
    AttrDecl [Attribute]
  deriving (Show, Eq)

-- | A pair of declarator and optional initializer.
data DeclInit
  = -- | Fully determined type for each declarator, and optional initializer.
    DeclInit
    { -- | Fully determined type.
      _decl_type :: Type,
      -- | Storage class.
      _decl_stor :: StorageClass,
      -- | Initializer (optional).
      _decl_init :: Maybe Initializer
    }
  deriving (Show, Eq)

-- | A @static\_assertion@.
data StaticAssertion
  = -- | An expression that must not evaluate to 0, and optional message
    -- that should be emitted if it does.
    StaticAssertion
    { _sa_expr :: ConstIntExpr,
      _sa_mesg :: (Maybe StringLiteral)
    }
  deriving (Show, Eq)

-- | A C storage class specifier
data StorageClass
  = SExtern
  | SAuto
  | SStatic
  | STypedef
  | SRegister
  | SThreadLocal
  | SConstExpr
  deriving (Show, Eq)

-- *** C Expressions

-- | A C expression. We pay attention to have only up to four constructors.
data Expr
  = Expr PrimExpr
  | Expr1 UnaryOp Expr
  | Expr2 BinOp Expr Expr
  | ExprSpecial SpecialExpr
  deriving (Show, Eq)

pattern ExprPostInc, ExprPostDec, ExprPreInc, ExprPreDec :: Expr -> Expr
pattern ExprPostInc e = Expr1 UOPostInc e
pattern ExprPostDec e = Expr1 UOPostDec e
pattern ExprPreInc e = Expr1 UOPreInc e
pattern ExprPreDec e = Expr1 UOPreDec e

pattern ExprUnaryPlus, ExprUnaryMinus, ExprLogNot :: Expr -> Expr
pattern ExprUnaryPlus e = Expr1 UOPlus e
pattern ExprUnaryMinus e = Expr1 UOMinus e
pattern ExprLogNot e = Expr1 UOLogNot e

pattern ExprBitNot, ExprDeref, ExprAddrOf :: Expr -> Expr
pattern ExprBitNot e = Expr1 UOBitNot e
pattern ExprDeref e = Expr1 UODeref e
pattern ExprAddrOf e = Expr1 UOAddrOf e

pattern ExprIndex, ExprTimes, ExprDiv :: Expr -> Expr -> Expr

-- | @a[b]@ gets desugared into @*(a + b)@.
pattern ExprIndex e f = ExprDeref (ExprPlus e f)

pattern ExprTimes e f = Expr2 BOTimes e f

pattern ExprDiv e f = Expr2 BODiv e f

pattern ExprMod, ExprPlus, ExprMinus :: Expr -> Expr -> Expr
pattern ExprMod e f = Expr2 BOMod e f
pattern ExprPlus e f = Expr2 BOPlus e f
pattern ExprMinus e f = Expr2 BOMinus e f

pattern ExprShiftL, ExprShiftR, ExprLT :: Expr -> Expr -> Expr
pattern ExprShiftL e f = Expr2 BOShiftL e f
pattern ExprShiftR e f = Expr2 BOShiftR e f
pattern ExprLT e f = Expr2 BOLT e f

pattern ExprGT, ExprLE, ExprGE :: Expr -> Expr -> Expr
pattern ExprGT e f = Expr2 BOGT e f
pattern ExprLE e f = Expr2 BOLE e f
pattern ExprGE e f = Expr2 BOGE e f

pattern ExprEQ, ExprNE, ExprBitAnd :: Expr -> Expr -> Expr
pattern ExprEQ e f = Expr2 BOEQ e f
pattern ExprNE e f = Expr2 BONE e f
pattern ExprBitAnd e f = Expr2 BOBitAnd e f

pattern ExprBitXor, ExprBitOr, ExprLogAnd :: Expr -> Expr -> Expr
pattern ExprBitXor e f = Expr2 BOBitXor e f
pattern ExprBitOr e f = Expr2 BOBitOr e f
pattern ExprLogAnd e f = Expr2 BOLogAnd e f

pattern ExprLogOr, ExprAssign, ExprAssignPlus :: Expr -> Expr -> Expr
pattern ExprLogOr e f = Expr2 BOLogOr e f
pattern ExprAssign e f = Expr2 BOAssign e f
pattern ExprAssignPlus e f = Expr2 BOAssignPlus e f

pattern ExprAssignMinus, ExprAssignTimes, ExprAssignDiv :: Expr -> Expr -> Expr
pattern ExprAssignMinus e f = Expr2 BOAssignMinus e f
pattern ExprAssignTimes e f = Expr2 BOAssignTimes e f
pattern ExprAssignDiv e f = Expr2 BOAssignDiv e f

pattern ExprAssignShiftL, ExprAssignShiftR :: Expr -> Expr -> Expr
pattern ExprAssignShiftL e f = Expr2 BOAssignShiftL e f
pattern ExprAssignShiftR e f = Expr2 BOAssignShiftR e f

pattern ExprAssignBitAnd :: Expr -> Expr -> Expr
pattern ExprAssignBitAnd e f = Expr2 BOAssignBitAnd e f

pattern ExprAssignBitOr, ExprSequence :: Expr -> Expr -> Expr
pattern ExprAssignBitOr e f = Expr2 BOAssignBitOr e f
pattern ExprSequence e f = Expr2 BOSequence e f

pattern ExprCall :: Expr -> [Expr] -> Expr
pattern ExprCall e fs = ExprSpecial (SECall e fs)

pattern ExprMember, ExprMemberPtr :: Expr -> Symbol -> Expr
pattern ExprMember e s = ExprSpecial (SEMember e s)

-- | @a->b@ gets desugared into @(*a).b@.
pattern ExprMemberPtr e s = ExprDeref e `ExprMember` s

pattern ExprCompoundLiteral :: Type -> Initializer -> Expr
pattern ExprCompoundLiteral t i = ExprSpecial (SECompoundLiteral t i)

pattern ExprSizeof :: (Either Expr Type) -> Expr
pattern ExprSizeof et = ExprSpecial (SESizeof et)

pattern ExprAlignof :: (Either ConstIntExpr Type) -> Expr
pattern ExprAlignof et = ExprSpecial (SEAlignof et)

-- | @a ? b : c@
pattern ExprITE :: Expr -> Expr -> Expr -> Expr
pattern ExprITE e f g = ExprSpecial (SEITE e f g)

pattern ExprId :: Name -> Expr
pattern ExprId name = Expr (PrimId name)

pattern ExprLit :: Lit -> Expr
pattern ExprLit l = Expr (PrimLit l)

pattern ExprParen :: Expr -> Expr
pattern ExprParen p = Expr (PrimParen p)

pattern ExprGeneric :: Expr -> [GenAssoc] -> Expr
pattern ExprGeneric e gs = Expr (PrimGeneric e gs)

{-# COMPLETE
  ExprPostInc,
  ExprPostDec,
  ExprPreInc,
  ExprPreDec,
  ExprUnaryPlus,
  ExprUnaryMinus,
  ExprLogNot,
  ExprBitNot,
  ExprDeref,
  ExprAddrOf,
  ExprTimes,
  ExprDiv,
  ExprMod,
  ExprPlus,
  ExprMinus,
  ExprShiftL,
  ExprShiftR,
  ExprLT,
  ExprGT,
  ExprLE,
  ExprGE,
  ExprEQ,
  ExprNE,
  ExprBitAnd,
  ExprBitXor,
  ExprBitOr,
  ExprLogAnd,
  ExprLogOr,
  ExprAssign,
  ExprAssignPlus,
  ExprAssignMinus,
  ExprAssignTimes,
  ExprAssignDiv,
  ExprAssignShiftL,
  ExprAssignShiftR,
  ExprAssignBitAnd,
  ExprAssignBitOr,
  ExprSequence,
  ExprCall,
  ExprMember,
  ExprCompoundLiteral,
  ExprSizeof,
  ExprAlignof,
  ExprITE,
  ExprId,
  ExprLit,
  ExprParen,
  ExprGeneric
  #-}

-- | Generic unary operation
data UnaryOp
  = UOPostInc
  | UOPostDec
  | UOPreInc
  | UOPreDec
  | UOPlus
  | UOMinus
  | UOLogNot
  | UOBitNot
  | UODeref
  | UOAddrOf
  deriving (Show, Eq)

-- | Generic binary opration ('Expr' × 'Expr')
data BinOp
  = BOTimes
  | BODiv
  | BOMod
  | BOPlus
  | BOMinus
  | BOShiftL
  | BOShiftR
  | -- | Less than
    BOLT
  | -- | Greater than
    BOGT
  | -- | Less than or equal to
    BOLE
  | -- | Greater than or equal to
    BOGE
  | -- | Equal to
    BOEQ
  | -- | Not equal to
    BONE
  | BOBitAnd
  | BOBitXor
  | BOBitOr
  | BOLogAnd
  | BOLogOr
  | BOAssign
  | BOAssignPlus
  | BOAssignMinus
  | BOAssignTimes
  | BOAssignDiv
  | BOAssignMod
  | BOAssignShiftL
  | BOAssignShiftR
  | BOAssignBitAnd
  | BOAssignBitXor
  | BOAssignBitOr
  | -- | Comma (`,`)
    BOSequence
  deriving (Show, Eq)

-- | Expressions thare are not like anything else.
data SpecialExpr
  = -- | Function call. Function designator, and then arguments.
    SECall Expr [Expr]
  | -- | Member access. @a.b@. @a-\>b@ is desugared to @(\*a).b@.
    SEMember Expr Symbol
  | -- | Compound literal, annotated with type. @(type) {...}@.
    SECompoundLiteral Type Initializer
  | SESizeof (Either Expr Type)
  | -- | If-then-else expression (@?:@)
    SEITE Expr Expr Expr
  | SEAlignof (Either ConstIntExpr Type)
  deriving (Show, Eq)

-- | A primary expression.
data PrimExpr
  = PrimId Name
  | PrimLit Lit
  | PrimParen Expr
  | PrimGeneric Expr [GenAssoc]
  deriving (Show, Eq)

-- | A generic association, part of a generic selection. If no type given,
-- then it's the @default@ case.
data GenAssoc
  = GenAssoc
  { -- | Type given, or @default@ if 'Nothing'
    _ga_type :: (Maybe Type),
    -- | Expression.
    _ga_expr :: Expr
  }
  deriving (Show, Eq)

-- | A constant integer expression. Use 'mkconstintexpr' to
-- construct in 'IO' (or a 'MonadIO' type).
data ConstIntExpr
  = ConstIntExpr
  { -- | The expression
    _ci_expr :: Expr,
    -- | Resolved value, if any
    _ci_refval :: (IORef (Maybe Integer))
  }

-- *** C Statements

-- | A C statement.
type Statement = StmtBody

-- | A C statement.
data StmtBody
  = -- | Any expression statement can be preceded with attributes.
    StmtExpr [Attribute] (Maybe Expr)
  | -- | @{ statements and declarations }@.
    StmtCompound (Seq BlockItem)
  | StmtIf Expr Statement (Maybe Statement)
  | -- | A @switch (expression) statement@ consists of an expression to
    -- branch and a statement that contains special @case@ and @default@ labels.
    -- However, formally, any statement can go. Thus, @switch(0)puts("");@ is
    -- legal, well-defined, and does exactly nothing.
    StmtSwitch Expr Statement
  | StmtWhile Expr Statement
  | StmtDoWhile Statement Expr
  | -- | The first clause of a @for@ loop statement can be either an expression
    -- or a declaration.
    StmtFor ForInit (Maybe Expr) (Maybe Expr) Statement
  | -- | @return@, @continue@, @break@, or @goto@.
    StmtJump Jump
  | -- | Since C23, a label can officially be attached to a null statement.
    StmtLabeled Label (Maybe Statement)
  deriving (Show, Eq)

-- | A 'BlockItem' can either be a declaration, a statement, or a label.
-- A standalone label is translated as a labeled null statement.
data BlockItem
  = BIDecl Declaration
  | BIStmt Statement
  deriving (Show, Eq)

-- | A 'label' can be given by an identifier, a @case \_:@, or @default:@.
data Label
  = LabelNamed [Attribute] Symbol
  | LabelCase [Attribute] Symbol ConstIntExpr
  | LabelDefault [Attribute] Symbol
  deriving (Show, Eq)

-- | The first clause of a @for@ declaration can either be a declaration
-- or an expression. The expression can be null.
data ForInit = FIDecl Declaration | FIExpr (Maybe Expr)
  deriving (Show, Eq)

-- | A jump can be either a @return \_@, @break@, @continue@,
-- or a @goto \_@.
data Jump
  = JumpReturn (Maybe Expr)
  | JumpBreak
  | JumpContinue
  | JumpGoto Name
  deriving (Show, Eq)

-- *** C Initializers

-- | An initializer.
data Initializer = InitExpr Expr | InitBraced [InitItem]
  deriving (Show, Eq)

-- | Member of a braced initializer.
data InitItem = InitItem
  { -- | Example: @.vector[2][3].x@ (followed by @=@ and then value).
    _init_designation :: [Designator],
    -- | Value. It recurses.
    _init_value :: Initializer
  }
  deriving (Show, Eq)

-- | A C designator may be an array index or a member access.
data Designator
  = -- | Syntax: @[constant-expression]@
    DesignatorIndex ConstIntExpr
  | -- | Syntax: @.identifier@
    DesignatorMember Name
  deriving (Show, Eq)

-- ** C Literals

-- | A C Literal
data Lit
  = -- | Integer literal
    LitInteger IntegerLiteral
  | -- | Character constant
    LitChar CharacterLiteral
  | -- | String literal
    LitString StringLiteral
  deriving (Show, Eq)

-- | An integer literal, annotated with type
data IntegerLiteral
  = IntegerLiteral Integer Prim
  deriving (Show, Eq)

-- | A character literal, annotated with type
data CharacterLiteral
  = -- | Literal character or universal character name, encoding choice delayed.
    CharacterLiteral Char Prim
  | -- | Specified in octal or hexadecimal.
    IntCharacterLiteral Integer Prim
  deriving (Show, Eq)

-- | A string literal; escape sequences have been interpreted, but
-- a NUL byte has NOT been inserted at the end. The encoding is:
--
--  - UTF-8 for single-byte-character strings
--  - UTF-16 for two-byte-character strings
--  - UTF-32 for four-byte-character strings
data StringLiteral
  = StringLiteral
  { -- | We use a 'LazyByteString' to minimize overhead of
    -- manipulating them while being able to show and determine
    -- equality with them easily.
    _strlit_buf :: LazyByteString,
    -- | Character type.
    _strlit_chartype :: Prim
  }
  deriving (Show, Eq)

-- | It will merely say @\<symbol\>@.
instance Show Symbol where
  show _ = "<symbol>"

instance Hashable Symbol where
  hashWithSalt salt sym = hashWithSalt @Int salt (coerce hashUnique sym)

-- | It will merely say @\<declarator\>@.
instance Show Declarator where
  show _ = "<declarator>"

instance Show ConstIntExpr where
  showsPrec d (ConstIntExpr e _) =
    showParen (d > 10) do
      ("ConstIntExpr " ++) . showsPrec (d + 1) e

instance Eq ConstIntExpr where
  (==) = (==) `on` _ci_expr

-- | A qualifier basis
_qu_none, _qu_const, _qu_restrict, _qu_volatile, _qu_atomic :: Qual
_qu_none = mempty
_qu_const = Qual 1
_qu_restrict = Qual 2
_qu_volatile = Qual 4
_qu_atomic = Qual 8

__bool :: (Bits a, Monoid a) => a -> Lens' a Bool
__bool mask = lens getter setter
 where
  getter = (/= mempty) . (.&. mask)
  setter a = \case
    True -> a .|. mask
    _ -> a .&. complement mask

-- | Does the qualifier set have ...?
qu_const, qu_restrict, qu_volatile, qu_atomic :: Lens' Qual Bool
qu_const = __bool _qu_const
qu_restrict = __bool _qu_restrict
qu_volatile = __bool _qu_volatile
qu_atomic = __bool _qu_atomic

qu_any, qu_none :: Getter Qual Bool

-- | Has any qualifier been set?
qu_any = to (/= mempty)

-- | Is it equal to '_qu_none'?
qu_none = to (== mempty)

-- * Some extra lenses

lab_attrs :: Lens' Label [Attribute]
lab_attrs = lens getter setter
 where
  getter = \case
    LabelNamed as _ -> as
    LabelCase as _ _ -> as
    LabelDefault as _ -> as
  setter = \case
    LabelNamed _ b -> (`LabelNamed` b)
    LabelCase _ b c -> \a -> LabelCase a b c
    LabelDefault _ b -> (`LabelDefault` b)

lab_sym :: Lens' Label Symbol
lab_sym = lens getter setter
 where
  getter = \case
    LabelNamed _ s -> s
    LabelCase _ s _ -> s
    LabelDefault _ s -> s
  setter = \case
    LabelNamed as _ -> LabelNamed as
    LabelCase as _ c -> \b -> LabelCase as b c
    LabelDefault as _ -> LabelDefault as

-- | Find the 'Prim' type for an element. For 'StringLiteral's,
-- the returned type is the character type that corresponds to it.
lit_prim :: Lens' Lit Prim
lit_prim = lens getter setter
 where
  getter = \case
    LitInteger (IntegerLiteral _ p) -> p
    LitChar (CharacterLiteral _ p) -> p
    LitChar (IntCharacterLiteral _ p) -> p
    LitString (StringLiteral _ p) -> p
  setter = \case
    LitInteger (IntegerLiteral a _) -> LitInteger . IntegerLiteral a
    LitChar cl ->
      LitChar . case cl of
        CharacterLiteral a _ -> CharacterLiteral a
        IntCharacterLiteral a _ -> IntCharacterLiteral a
    LitString (StringLiteral a _) -> LitString . StringLiteral a

-- | Construct a new 'ConstIntExpr'.
mkconstintexpr :: (MonadIO m) => Expr -> m ConstIntExpr
mkconstintexpr e = ConstIntExpr e <$> newIORef Nothing

-- Generate lenses for all record types
makeLenses ''Type
makeLenses ''Attribute
makeLenses ''RecInfo
makeLenses ''ArrayInfo
makeLenses ''ParamArrayInfo
makeLenses ''EnumInfo
makeLenses ''EnumConst
makeLenses ''FuncInfo
makeLenses ''DeclInit
makeLenses ''InitItem
makeLenses ''StaticAssertion
