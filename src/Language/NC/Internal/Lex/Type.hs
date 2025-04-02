module Language.NC.Internal.Lex.Type (
  -- * Types
  Type (..),
  BaseType (..),

  -- * Type Qualifiers
  TypeQual (..),
  tq_none,
  tq_const,
  tq_volatile,
  tq_restrict,

  -- * Storage Classes
  StorageClass (..),
  sc_none,
  sc_register,
  sc_auto,
  sc_static,
  sc_extern,
  sc_threadlocal,
  sc_typedef,

  -- * Function Specifiers
  FuncSpec (..),
  fs_none,
  fs_inline,
  fs_noreturn,
  fs_atomic,

  -- * Alignment
  Alignment (..),

  -- * Supplemental information
  Record (..),
  RecordInfo (..),
  RecordField (..),
  EnumType (..),
  EnumInfo (..),
  EnumConst (..),
  FuncType (..),
  FuncInfo (..),
  Param (..),
  ArrayType (..),
  ArraySize (..),
  QualifiedType (..),

  -- * Parsing an expression determining type
  parsetype,
) where

import Data.List (intercalate)
import Language.NC.Internal.Lex.Op
import Language.NC.Internal.PrimTypes (PrimType)
import Language.NC.Internal.Prelude

-- in this parser-lexer we parse type names.

-- we also define C types.

-- | Source storage class monoid.
newtype StorageClass = StorageClass {unstorclass :: Int}
  deriving (Eq, Ord, Bits, FiniteBits)

instance Show StorageClass where
  show (StorageClass s) = case s of
    0 -> "none"
    _ ->
      intercalate " "
        $ filter
          (not . null)
          [ if s .&. unstorclass sc_register /= 0 then "register" else "",
            if s .&. unstorclass sc_auto /= 0 then "auto" else "",
            if s .&. unstorclass sc_static /= 0 then "static" else "",
            if s .&. unstorclass sc_extern /= 0 then "extern" else "",
            if s .&. unstorclass sc_threadlocal /= 0 then "_Thread_local" else "",
            if s .&. unstorclass sc_typedef /= 0 then "typedef" else ""
          ]

instance Semigroup StorageClass where
  StorageClass i <> StorageClass j = StorageClass (i .|. j)

instance Monoid StorageClass where
  mempty = StorageClass 0

sc_none, sc_register, sc_auto, sc_static :: StorageClass
sc_none = StorageClass 0
sc_register = StorageClass 1
sc_auto = StorageClass 2
sc_static = StorageClass 4

sc_extern, sc_threadlocal, sc_typedef :: StorageClass
sc_extern = StorageClass 8
sc_threadlocal = StorageClass 16
sc_typedef = StorageClass 32

-- | Source function specifier monoid.
newtype FuncSpec = FuncSpec {unfuncspec :: Int}
  deriving (Eq, Ord, Bits, FiniteBits)

instance Show FuncSpec where
  show (FuncSpec f) = case f of
    0 -> "none"
    _ ->
      intercalate " "
        $ filter
          (not . null)
          [ if f .&. unfuncspec fs_inline /= 0 then "inline" else "",
            if f .&. unfuncspec fs_noreturn /= 0 then "_Noreturn" else "",
            if f .&. unfuncspec fs_atomic /= 0 then "_Atomic" else ""
          ]

instance Semigroup FuncSpec where
  FuncSpec i <> FuncSpec j = FuncSpec (i .|. j)

instance Monoid FuncSpec where
  mempty = FuncSpec 0

fs_none, fs_inline, fs_noreturn, fs_atomic :: FuncSpec
fs_none = FuncSpec 0
fs_inline = FuncSpec 1
fs_noreturn = FuncSpec 2
fs_atomic = FuncSpec 4

-- | Source alignment specifier
data Alignment
  = AlignNone
  | -- | @\_Alignas@ with constant expression
    AlignAs Int
  | -- | @\_Alignas@ with type
    AlignAsType Type
  deriving (Eq, Show)

-- | Source base types.
data BaseType
  = -- | Primitive, non-derived type.
    BTPrim PrimType
  | -- | A record type, such as a struct or union.
    BTRecord Record
  | -- | An enumeration type.
    BTEnum EnumType
  | -- | A typedef name.
    BTTypeName Symbol
  | -- | A function type.
    BTFunc FuncType
  | -- | An array type.
    BTArray ArrayType
  | -- | A pointer type.
    BTPointer QualifiedType
  deriving (Eq, Show)

-- | A qualified type.
data QualifiedType
  = QualifiedType
  { qt_base :: BaseType,
    qt_qual :: TypeQual
  }
  deriving (Eq, Show)

data Record
  = -- | A struct type.
    RecordStruct Symbol RecordInfo
  | -- | A union type.
    RecordUnion Symbol RecordInfo
  deriving (Eq, Show)

data EnumType
  = -- | An enumeration type.
    EnumType Symbol EnumInfo
  deriving (Eq, Show)

data FuncType
  = -- | A function type.
    FuncType Symbol FuncInfo
  deriving (Eq, Show)

data ArrayType
  = -- | An array type.
    ArrayType ArraySize Type
  deriving (Eq, Show)

data ArraySize
  = -- | A fixed size array.
    ArraySize Int
  | -- | A variable length array.
    ArraySizeVar Expr
  | -- | A flexible array member or unknown size.
    ArraySizeNone
  deriving (Eq, Show)

-- | Monoid representing qualifier monoid.
newtype TypeQual = TypeQual {untypequal :: Int}
  deriving (Eq, Ord, Bits, FiniteBits)

instance Show TypeQual where
  show (TypeQual q) = case q of
    0 -> "none"
    _ ->
      intercalate " "
        $ filter
          (not . null)
          [ if q .&. untypequal tq_const /= 0 then "const" else "",
            if q .&. untypequal tq_volatile /= 0 then "volatile" else "",
            if q .&. untypequal tq_restrict /= 0 then "restrict" else ""
          ]

instance Semigroup TypeQual where
  TypeQual i <> TypeQual j = TypeQual (i .|. j)

instance Monoid TypeQual where
  mempty = TypeQual 0

tq_none, tq_const, tq_volatile, tq_restrict :: TypeQual
tq_none = TypeQual 0
tq_const = TypeQual 1
tq_volatile = TypeQual 2
tq_restrict = TypeQual 4

-- | Source types.
data Type = Type
  { ty_storclass :: StorageClass,
    ty_base :: BaseType,
    ty_qual :: TypeQual,
    ty_funcspec :: FuncSpec,
    ty_alignment :: Alignment
  }
  deriving (Eq, Show)

-- | Record information
data RecordInfo = RecordDef [RecordField] | RecordDecl
  deriving (Eq, Show)

-- | Record field
data RecordField = RecordField Type Symbol (Maybe Int)
  deriving (Eq, Show)

-- | Enum information
data EnumInfo = EnumDef [EnumConst] | EnumDecl
  deriving (Eq, Show)

-- | Enum constant
data EnumConst = EnumConst Symbol (Maybe Expr)
  deriving (Eq, Show)

-- | Function information
data FuncInfo = FuncDef [Param] Type | FuncDecl
  deriving (Eq, Show)

-- | Function parameter
data Param = Param Type Symbol | ParamUnnamed Type
  deriving (Eq, Show)

{- new info! C23 fragment (in PEG) of the type-name rule, which
parsetype represents:

type-name ← specifier-qualifier-list abstract-declarator?
abstract-declarator ← pointer tail? / tail
tail ← '(' abstract-declarator ')' suffix* / array attribute-specifier-sequence? / func attribute-specifier-sequence?
suffix ← '[' type-qualifier-list? assignment-expression? ']' / '[' 'static' type-qualifier-list? assignment-expression ']' / '[' type-qualifier-list 'static' assignment-expression ']' / '[' '*' ']' / '(' parameter-type-list? ')'
array ← '[' type-qualifier-list? assignment-expression? ']' / '[' 'static' type-qualifier-list? assignment-expression ']' / '[' type-qualifier-list 'static' assignment-expression ']' / '[' '*' ']'
func ← '(' parameter-type-list? ')'
-}

parsetype :: Parser Type
parsetype = error "parsetype: not implemented"
