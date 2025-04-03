{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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
  tq_atomic,

  -- * Storage Classes
  StorageClass (..),
  sc_none,
  sc_register,
  sc_auto,
  sc_static,
  sc_extern,
  sc_threadlocal,
  sc_typedef,
  sc_constexpr,

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
  Variadic (..),

  -- * Parsing an expression determining type
  parsetype,

  -- * Debugging
  typespecqual,
  typespecquals,
  SpecQual (..),
  TypeTokens (..),
) where

import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid
import Language.NC.Internal.Lex.Lex
import Language.NC.Internal.Lex.Op
import Language.NC.Internal.Prelude
import Language.NC.Internal.PrimTypes (PrimType)
import Language.NC.Internal.PrimTypes qualified as PT

-- in this parser-lexer we parse type names.

-- we also define C types.

-- | Source storage class monoid.
newtype StorageClass = StorageClass {unstorclass :: Int8}
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
            if s .&. unstorclass sc_typedef /= 0 then "typedef" else "",
            if s .&. unstorclass sc_constexpr /= 0 then "constexpr" else ""
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

sc_extern, sc_threadlocal, sc_typedef, sc_constexpr :: StorageClass
sc_extern = StorageClass 8
sc_threadlocal = StorageClass 16
sc_typedef = StorageClass 32
sc_constexpr = StorageClass 64

-- | Source function specifier monoid.
newtype FuncSpec = FuncSpec {unfuncspec :: Int8}
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
  deriving (Eq, Show, Ord)

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
  | -- | An atomic type specifier (_Atomic(...))
    BTAtomic QualifiedType
  deriving (Eq, Show, Ord)

-- | A qualified type.
data QualifiedType
  = QualifiedType
  { qt_base :: BaseType,
    qt_qual :: TypeQual
  }
  deriving (Eq, Show, Ord)

data Record
  = -- | A struct type.
    RecordStruct Symbol RecordInfo
  | -- | A union type.
    RecordUnion Symbol RecordInfo
  deriving (Eq, Show, Ord)

data EnumType
  = -- | An enumeration type.
    EnumType Symbol EnumInfo
  deriving (Eq, Show, Ord)

data FuncType
  = -- | A function type.
    FuncType Symbol FuncInfo
  deriving (Eq, Show, Ord)

data ArrayType
  = -- | An array type.
    ArrayType ArraySize Type
  deriving (Eq, Show, Ord)

data ArraySize
  = -- | A fixed size array.
    ArraySize Int
  | -- | A variable length array.
    ArraySizeVar Expr
  | -- | A flexible array member or unknown size.
    ArraySizeNone
  deriving (Eq, Show, Ord)

-- | Monoid representing qualifier monoid.
newtype TypeQual = TypeQual {untypequal :: Int8}
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
            if q .&. untypequal tq_restrict /= 0 then "restrict" else "",
            if q .&. untypequal tq_atomic /= 0 then "_Atomic" else ""
          ]

instance Semigroup TypeQual where
  TypeQual i <> TypeQual j = TypeQual (i .|. j)

instance Monoid TypeQual where
  mempty = TypeQual 0

tq_none, tq_const, tq_volatile, tq_restrict, tq_atomic :: TypeQual
tq_none = TypeQual 0
tq_const = TypeQual 1
tq_volatile = TypeQual 2
tq_restrict = TypeQual 4
tq_atomic = TypeQual 8

-- | Source types.
--
-- We also include the storage class declaration for convenience in
-- parsing and organization, though it is not part of the type itself.
data Type = Type
  { ty_storclass :: StorageClass,
    ty_base :: BaseType,
    ty_qual :: TypeQual,
    ty_funcspec :: FuncSpec,
    ty_alignment :: Alignment
  }
  deriving (Eq, Show, Ord)

-- | Record information
data RecordInfo = RecordDef [RecordField] | RecordDecl
  deriving (Eq, Show, Ord)

-- | Record field
data RecordField = RecordField Type Symbol (Maybe Int)
  deriving (Eq, Show, Ord)

-- | Enum information
data EnumInfo = EnumDef [EnumConst] | EnumDecl
  deriving (Eq, Show, Ord)

-- | Enum constant
data EnumConst = EnumConst Symbol (Maybe Expr)
  deriving (Eq, Show, Ord)

-- | Is a function variadic?
data Variadic = Variadic | NotVariadic
  deriving (Eq, Show, Ord)

-- | Function information
data FuncInfo = FuncDef [Param] Type Variadic | FuncDecl
  deriving (Eq, Show, Ord)

-- | Function parameter
data Param = Param Type Symbol | ParamUnnamed Type
  deriving (Eq, Show, Ord)

{- Optimized C23 fragment (in PEG) of the type-name rule for parsetype implementation:

# Fully inlined and optimized for PEG parsing efficiency
type-name ←
  # Step 1: Parse all type specifiers and qualifiers - collect in a list/set
  type-specifier-qualifier+

  # Step 2: Parse optional abstract declarator which transforms the base type
  (
    # Pointer chain followed by optional direct part
    ('*' type-qualifier* pointer? direct-abstract-declarator?) /

    # Just direct part without pointer prefix
    direct-abstract-declarator
  )?

# Optimized direct abstract declarator with declarator suffixes inlined for fewer productions
direct-abstract-declarator ←
  # Initial direct declarator fragment
  (
    # Case 1: Parenthesized abstract declarator
    '(' abstract-declarator ')' /

    # Case 2: Array declarator - all array forms inlined
    '[' (
      # Standard array form
      type-qualifier* assignment-expression? /
      # Static forms
      'static' type-qualifier* assignment-expression /
      type-qualifier+ 'static' assignment-expression /
      # VLA marker
      '*'
    ) ']' /

    # Case 3: Function declarator - parameter list inlined
    '(' (
      # Parameter list with optional variadic
      (declaration-specifiers abstract-declarator?
        (',' declaration-specifiers abstract-declarator?)*
        (',' '...')?)
      / '...'
    )? ')'
  )

  # Followed by zero or more additional declarator operations
  (
    # Array suffix form - all forms inlined
    '[' (
      type-qualifier* assignment-expression? /
      'static' type-qualifier* assignment-expression /
      type-qualifier+ 'static' assignment-expression /
      '*'
    ) ']' /

    # Function suffix form
    '(' (
      (declaration-specifiers abstract-declarator?
        (',' declaration-specifiers abstract-declarator?)*
        (',' '...')?)
      / '...'
    )? ')'
  )*

# Type specifier or qualifier - any component of specifier-qualifier-list
type-specifier-qualifier ←
  # Type specifiers (inlined key keywords)
  'void' / 'char' / 'short' / 'int' / 'long' / 'float' / 'double' /
  'signed' / 'unsigned' / '_Bool' / '_Complex' / '_Decimal32' / '_Decimal64' / '_Decimal128' /
  # Compound type specifiers
  struct-or-union-specifier / enum-specifier / atomic-type-specifier /
  typeof-specifier / typedef-name /
  # Type qualifiers (inlined)
  'const' / 'restrict' / 'volatile' / '_Atomic' /
  # Alignment specifiers
  alignment-specifier

== Implementation strategy ==

1. Parse all specifiers and qualifiers, building a base type and qualifier set
   - Collect all tokens that form valid type specifiers/qualifiers
   - Merge them according to C type combination rules (e.g. unsigned + int)
   - Build the initial Type node with these collected specifiers

2. If an abstract declarator is present, transform the base type:
   - For pointers: wrap the current type in BTPointer nodes for each level
   - For arrays: wrap in BTArray with size information
   - For functions: wrap in BTFunc with parameter information
   - Apply in correct order from innermost to outermost

3. Apply qualifiers at appropriate levels:
   - Pointer qualifiers attach to the pointer level
   - Array/function qualifiers attach to the respective type node

4. Handle special case combinations:
   - Atomic type expressions: _Atomic(int) vs _Atomic as a qualifier
   - Function pointer types: building correct nesting structure
   - Array of arrays: ensuring proper dimension ordering

Key point: Rather than building a CST and then walking it, transform the Type node
directly as parsing proceeds, from the base type outward.
-}

-- as you can see down here, quite a lot of effort was spent in
-- the permutation parsing of type qualifiers and specifiers.

-- Type specifier token - used to count occurrences in parsing
data TSTok
  = -- Type specifiers
    TStVoid -- void
  | TStChar -- char
  | TStShort -- short
  | TStInt -- int
  | TStLong -- long
  | TStFloat -- float
  | TStDouble -- double
  | TStSigned -- signed
  | TStUnsigned -- unsigned
  | TStBool -- _Bool
  | TStComplex -- _Complex
  | -- C23 types
    TStDecimal32 -- _Decimal32
  | TStDecimal64 -- _Decimal64
  | TStDecimal128 -- _Decimal128
  | TStBitInt -- _BitInt
  | -- Type qualifiers
    TStConst -- const
  | TStVolatile -- volatile
  | TStRestrict -- restrict
  | TStAtomic -- _Atomic
  | -- Storage class specifiers
    TStRegister -- register
  | TStAuto -- auto
  | TStStatic -- static
  | TStExtern -- extern
  | TStThreadLocal -- _Thread_local
  | TStTypedef -- typedef
  | TStConstexpr -- constexpr
  | -- Function specifiers
    TStInline -- inline
  | TStNoreturn -- _Noreturn
  deriving (Eq, Enum, Show, Ord)

-- Bitset for type specifier tokens
newtype TSToks = TSToks {untstoks :: Word32}
  deriving (Eq, Ord, Num, Bits, FiniteBits)

instance Semigroup TSToks where
  TSToks i <> TSToks j = TSToks (i .|. j)

instance Monoid TSToks where
  mempty = TSToks 0

instance Show TSToks where
  show toks =
    case joinwords [storage, function, qualifiers, specifiers] of
      "" -> "<no type specifiers or qualifiers>"
      s -> s
   where
    ifonly b c
      | (toks .&. b) /= 0 = c
      | otherwise = ""
    joinwords = unwords . filter (not . null)
    storage =
      joinwords
        [ ifonly tst_register "register",
          ifonly tst_auto "auto",
          ifonly tst_static "static",
          ifonly tst_extern "extern",
          ifonly tst_threadlocal "_Thread_local",
          ifonly tst_typedef "typedef",
          ifonly tst_constexpr "constexpr"
        ]
    function =
      joinwords
        [ ifonly tst_inline "inline",
          ifonly tst_noreturn "_Noreturn"
        ]
    qualifiers =
      joinwords
        [ ifonly tst_const "const",
          ifonly tst_volatile "volatile",
          ifonly tst_restrict "restrict",
          ifonly tst_atomic "_Atomic"
        ]
    specifiers =
      joinwords
        [ -- Modifiers
          ifonly tst_signed "signed",
          ifonly tst_unsigned "unsigned",
          -- Base types
          ifonly tst_void "void",
          ifonly tst_char "char",
          ifonly tst_short "short",
          case toks .&. (tst_long .|. tst_longlong) of
            t
              | t == tst_long -> "long"
              | t == tst_longlong -> "long long"
              | otherwise -> "",
          ifonly tst_double "double",
          ifonly tst_int "int",
          ifonly tst_float "float",
          ifonly tst_bool "_Bool",
          ifonly tst_complex "_Complex",
          ifonly tst_decimal32 "_Decimal32",
          ifonly tst_decimal64 "_Decimal64",
          ifonly tst_decimal128 "_Decimal128",
          ifonly tst_bitint "_BitInt(...)"
        ]

-- long is treated separately
tst_void, tst_char, tst_short, tst_int :: TSToks
tst_void = TSToks (1 .<<. 0)
tst_char = TSToks (1 .<<. 1)
tst_short = TSToks (1 .<<. 2)
tst_int = TSToks (1 .<<. 3)

-- long long vs. long

-- two longs may appear, so top two bits are used to indicate the number.
--
-- - 00: <nothing>
-- - 01: long
-- - 11: long long
tst_long, tst_longlong :: TSToks
tst_long = TSToks (1 .<<. 4)
tst_longlong = TSToks (3 .<<. 4)

tst_float, tst_double, tst_signed, tst_unsigned, tst_bool :: TSToks
tst_float = TSToks (1 .<<. 6)
tst_double = TSToks (1 .<<. 7)
tst_signed = TSToks (1 .<<. 8)
tst_unsigned = TSToks (1 .<<. 9)
tst_bool = TSToks (1 .<<. 10)

tst_complex, tst_decimal32, tst_decimal64, tst_decimal128 :: TSToks
tst_complex = TSToks (1 .<<. 11)
tst_decimal32 = TSToks (1 .<<. 12)
tst_decimal64 = TSToks (1 .<<. 13)
tst_decimal128 = TSToks (1 .<<. 14)

tst_bitint :: TSToks
tst_bitint = TSToks (1 .<<. 15)

tst_const, tst_volatile, tst_restrict :: TSToks
tst_const = TSToks (1 .<<. 16)
tst_volatile = TSToks (1 .<<. 17)
tst_restrict = TSToks (1 .<<. 18)

tst_atomic, tst_register :: TSToks
tst_atomic = TSToks (1 .<<. 19)
tst_register = TSToks (1 .<<. 20)

tst_auto, tst_static, tst_extern :: TSToks
tst_auto = TSToks (1 .<<. 21)
tst_static = TSToks (1 .<<. 22)
tst_extern = TSToks (1 .<<. 23)

tst_threadlocal, tst_typedef :: TSToks
tst_threadlocal = TSToks (1 .<<. 24)
tst_typedef = TSToks (1 .<<. 25)

tst_constexpr, tst_inline, tst_noreturn :: TSToks
tst_constexpr = TSToks (1 .<<. 26)
tst_inline = TSToks (1 .<<. 27)
tst_noreturn = TSToks (1 .<<. 28)

-- | Container for holding all information collected during type parsing
data TypeTokens = TypeTokens
  { -- | Count of each token type.
    -- This is for error reporting
    -- if user uses too many specifiers and qualifiers.
    _tt_counts :: !(Map TSTok Int),
    -- | The real deal.
    _tt_mask :: !TSToks,
    -- | Bit width for _BitInt, if any
    _tt_bitintwidth :: !(Maybe Int)
  }
  deriving (Eq, Show)

makeLenses ''TypeTokens

-- if type found, find base type. otherwise, report error.
tt2basic :: TSToks -> Parser BaseType
tt2basic v =
  -- we shouldn't bother with other specifiers.
  fmap BTPrim case ((1 .<<. 16) - 1) .&. v of
    -- void
    0x0001 -> pure PT.Void
    -- char variants
    0x0002 -> pure PT.Char_ -- char
    0x0102 -> pure PT.SChar_ -- signed char
    0x0202 -> pure PT.UChar_ -- unsigned char
    -- int variants (0x8)
    0x0008 -> pure PT.Int_ -- (sim.)
    0x0108 -> pure PT.Int_
    0x0208 -> pure PT.UInt_
    -- short variants
    0x0004 -> pure PT.Short_ -- short
    0x000c -> pure PT.Short_ -- short int
    0x0104 -> pure PT.Short_ -- signed short
    0x010c -> pure PT.Short_ -- signed short int
    0x0204 -> pure PT.UShort_ -- (sim.)
    0x020c -> pure PT.UShort_ -- (sim.)
    -- long variants
    0x0010 -> pure PT.Long_ -- long
    0x0018 -> pure PT.Long_ -- long int
    0x0110 -> pure PT.Long_ -- (etc.)
    0x0118 -> pure PT.Long_
    0x0210 -> pure PT.ULong_
    0x0218 -> pure PT.ULong_
    -- long long variants
    0x0030 -> pure PT.LongLong_ -- (sim.)
    0x0038 -> pure PT.LongLong_
    0x0130 -> pure PT.LongLong_
    0x0138 -> pure PT.LongLong_
    0x0230 -> pure PT.ULongLong_
    0x0238 -> pure PT.ULongLong_
    -- floating point types
    0x0040 -> pure PT.Float_
    0x0080 -> pure PT.Double_
    0x0090 -> pure PT.LongDouble_
    -- complex types
    0x0840 -> pure PT.ComplexFloat_
    0x0880 -> pure PT.ComplexDouble_
    0x0890 -> pure PT.ComplexLongDouble_
    -- bool
    0x0400 -> pure PT.Bool
    -- decimal types
    0x1000 -> pure $ PT.Float (PT.Real PT.RFDecimal32)
    0x2000 -> pure $ PT.Float (PT.Real PT.RFDecimal64)
    0x4000 -> pure $ PT.Float (PT.Real PT.RFDecimal128)
    -- bit int types
    0x8000 -> pure $ PT.Int PT.Signed (PT.BitInt 0)
    0x8100 -> pure $ PT.Int PT.Signed (PT.BitInt 0)
    0x8200 -> pure $ PT.Int PT.Unsigned (PT.BitInt 0)
    -- error case
    _ ->
      err
        $ BasicError
        $ "Unknown type token combination %s"
        ++ show v

-- add to the type token counts
newtype SpecQual = SpecQual {runspecqual :: TypeTokens -> TypeTokens}
  deriving (Monoid, Semigroup) via (Endo TypeTokens)

-- | Parse the "type-specifier-qualifier" rule
typespecqual :: Parser SpecQual
typespecqual = do
  $( switch_ws1
       [|
         case _ of
           "void" -> push TStVoid tst_void
           "char" -> push TStChar tst_char
           "short" -> push TStShort tst_short
           "int" -> push TStInt tst_int
           "long" -> handlelong
           "float" -> push TStFloat tst_float
           "double" -> push TStDouble tst_double
           "signed" -> push TStSigned tst_signed
           "unsigned" -> push TStUnsigned tst_unsigned
           "_Bool" -> push TStBool tst_bool
           "_Complex" -> push TStComplex tst_complex
           "_Decimal32" -> push TStDecimal32 tst_decimal32
           "_Decimal64" -> push TStDecimal64 tst_decimal64
           "_Decimal128" -> push TStDecimal128 tst_decimal128
           "_BitInt" -> lx0 (inpar $ decimal) >>= push_bitint
           "const" -> push TStConst tst_const
           "volatile" -> push TStVolatile tst_volatile
           "restrict" -> push TStRestrict tst_restrict
           "_Atomic" -> push TStAtomic tst_atomic
           "register" -> push TStRegister tst_register
           "auto" -> push TStAuto tst_auto
           "static" -> push TStStatic tst_static
           "extern" -> push TStExtern tst_extern
           "_Thread_local" -> push TStThreadLocal tst_threadlocal
           "typedef" -> push TStTypedef tst_typedef
           "constexpr" -> push TStConstexpr tst_constexpr
           "inline" -> push TStInline tst_inline
           "_Noreturn" -> push TStNoreturn tst_noreturn
         |]
   )
 where
  decimal =
    integer_constant_val >>= \case
      IntegerLiteral n _ -> pure $ fromIntegral n
  prepush token mask =
    over tt_counts (Map.insertWith (+) token 1)
      . over tt_mask (<> mask)
  push token mask = pure $ SpecQual $ prepush token mask
  push_bitint width =
    pure
      $ SpecQual (prepush TStBitInt tst_bitint)
      <> SpecQual (set tt_bitintwidth (Just width))
  handlelong = pure $ SpecQual \tt ->
    -- if we pushed it before then we use a different bit.
    case Map.lookup TStLong (_tt_counts tt) of
      Nothing -> prepush TStLong tst_long tt
      Just _ -> prepush TStLong tst_longlong tt

typespecquals :: Parser BaseType
typespecquals = do
  SpecQual f <- chainl (<>) typespecqual typespecqual
  let tt = f $ TypeTokens mempty mempty Nothing
  checksanity tt
  bt <- tt2basic (_tt_mask tt)
  case tt ^. tt_bitintwidth of
    Nothing -> pure bt
    Just bw
      | 0 < bw && bw < 256 -> case bt of
          -- as of writing, no profunctor for this yet.
          -- FIXME: add validation.
          BTPrim (PT.Int signed (PT.BitInt _)) ->
            pure $ BTPrim $ PT.Int signed (PT.BitInt $ fromIntegral bw)
          _ -> err $ InternalError "typespecquals: Nothing"
      | otherwise ->
          err
            $ InternalError
              ("typespecquals: bad _BitInt width " ++ show bw)
 where
  badcondition TStLong c = c > 2
  badcondition _ c = c > 1
  checksanity tt = case tt ^. tt_counts of
    cc -> case Map.filterWithKey badcondition cc of
      map2
        | Map.null map2 -> pure ()
        | otherwise -> err $ BasicError "too many specifiers"

-- | Parse types
parsetype :: Parser Type
parsetype = err $ InternalError "parsetype not implemented yet"
