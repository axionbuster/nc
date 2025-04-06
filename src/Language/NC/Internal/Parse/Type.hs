{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Language.NC.Internal.Parse.Type (
  -- * Parsing an expression determining type
  parsetype,

  -- * Debugging
  typespecqual,
  typespecquals,
  SpecQual (..),
  TypeTokens (..),
) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Monoid
import Language.NC.Internal.Lex
import {-# SOURCE #-} Language.NC.Internal.Parse.Op
import Language.NC.Internal.Prelude hiding (L1, assign)

-- in this parser-lexer we parse type names.

-- we also define C types.

{- Optimized C23 fragment (in PEG) of the type-name rule for parsetype implementation:

# Fully inlined and optimized for PEG parsing efficiency
# Type name - entry point for parsing C types
type-name ←
  # First collect all type specifiers and qualifiers
  type-specifier-qualifier+ attribute-specifier-sequence?
  # Then optionally parse abstract declarator
  (
    # Case 1: Pointer chain followed by optional direct part
    ('*' attribute-specifier-sequence? ('const'/'restrict'/'volatile'/'_Atomic')*)*
    (
      # Direct abstract declarator (parenthesized or array/function)
      (
        # Parenthesized abstract declarator
        '(' (
          # Either a full abstract declarator inside parentheses
          ('*' attribute-specifier-sequence? ('const'/'restrict'/'volatile'/'_Atomic'*)*
           (direct-abstract-declarator)?) /
          # Or just a direct abstract declarator
          direct-abstract-declarator
        ) ')' /

        # Array form with all variations in priority order
        '[' (
          'static' ('const'/'restrict'/'volatile'/'_Atomic')* assignment-expression /
          ('const'/'restrict'/'volatile'/'_Atomic')+ 'static' assignment-expression /
          ('const'/'restrict'/'volatile'/'_Atomic')* assignment-expression? /
          ('const'/'restrict'/'volatile'/'_Atomic')* '*'
        ) ']' attribute-specifier-sequence? /

        # Function form
        '(' (
          # Parameter list with variadic option
          parameter-declaration (',' parameter-declaration)* (',' '...')? /
          '...'
        )? ')' attribute-specifier-sequence?
      )

      # Suffix decorators (array or function)
      (
        # Array suffix
        '[' (
          'static' ('const'/'restrict'/'volatile'/'_Atomic')* assignment-expression /
          ('const'/'restrict'/'volatile'/'_Atomic')+ 'static' assignment-expression /
          ('const'/'restrict'/'volatile'/'_Atomic')* assignment-expression? /
          ('const'/'restrict'/'volatile'/'_Atomic')* '*'
        ) ']' attribute-specifier-sequence? /

        # Function suffix
        '(' (parameter-declaration (',' parameter-declaration)* (',' '...')? / '...')? ')'
         attribute-specifier-sequence?
      )*
    )?
  )?

# Direct abstract declarator - factored out due to mutual recursion
direct-abstract-declarator ←
  # Parenthesized abstract declarator
  '(' abstract-declarator ')' /

  # Array form
  '[' (
    'static' ('const'/'restrict'/'volatile'/'_Atomic')* assignment-expression /
    ('const'/'restrict'/'volatile'/'_Atomic')+ 'static' assignment-expression /
    ('const'/'restrict'/'volatile'/'_Atomic')* assignment-expression? /
    ('const'/'restrict'/'volatile'/'_Atomic')* '*'
  ) ']' attribute-specifier-sequence? /

  # Function form
  '(' (parameter-declaration (',' parameter-declaration)* (',' '...')? / '...')? ')'
   attribute-specifier-sequence?

# Abstract declarator - needed for parenthesized expressions
abstract-declarator ←
  ('*' attribute-specifier-sequence? ('const'/'restrict'/'volatile'/'_Atomic'*)*)+ direct-abstract-declarator? /
  direct-abstract-declarator

# Type specifier or qualifier - consolidated into one rule
type-specifier-qualifier ←
  # Built-in types (ordered for quick recognition)
  'void' / 'char' / 'short' / 'int' / 'long' / 'float' / 'double' /
  'signed' / 'unsigned' / '_Bool' / '_Complex' / '_Decimal32' / '_Decimal64' / '_Decimal128' /
  '_BitInt' '(' constant-expression ')' /
  # Type qualifiers (very common, parse early)
  'const' / 'restrict' / 'volatile' / '_Atomic' /
  # Compound type specifiers (more complex, parse later)
  ('struct' / 'union') attribute-specifier-sequence? (
    identifier? '{' (
      attribute-specifier-sequence? type-specifier-qualifier+ (
        (declarator? ':' constant-expression (',' declarator? ':' constant-expression)*) /
        (declarator (',' declarator)*) /
        ε
      ) ';' /
      'static_assert' '(' constant-expression (',' string-literal)? ')' ';'
    )+ '}' /
    identifier
  ) /
  'enum' attribute-specifier-sequence? identifier? ('{' enumerator-list ','? '}')? /
  '_Atomic' '(' type-name ')' /
  ('typeof' / 'typeof_unqual') '(' (type-name / expression) ')' /
  'alignas' '(' (type-name / constant-expression) ')' /
  typedef-name  # Check last to avoid conflicts with identifiers

# Attribute specifier sequence - simplified
attribute-specifier-sequence ←
  ('[' '[' attribute? (',' attribute?)* ']' ']')*

# Parameter type list - function parameters with optional variadic suffix
parameter-type-list ←
  # Case 1: One or more parameter declarations with optional variadic suffix
  parameter-declaration (',' parameter-declaration)* (',' '...')? /
  # Case 2: Purely variadic function (no named parameters)
  '...'

# Parameter declaration - simplified for type-name context
parameter-declaration ←
  attribute-specifier-sequence? type-specifier-qualifier+ (declarator / abstract-declarator)?

# Declarator - non-recursive form for PEG parsing
declarator ←
  # Step 1: Optional pointer chain
  ('*' attribute-specifier-sequence? ('const'/'restrict'/'volatile'/'_Atomic')*)*

  # Step 2: Base declarator (identifier or grouped declarator)
  (
    # Named identifier with attributes
    identifier attribute-specifier-sequence? /

    # Parenthesized declarator for grouping
    '(' declarator ')'
  )

  # Step 3: Array/function suffixes (zero or more)
  (
    # Array suffix forms - all variations in priority order
    '[' (
      'static' ('const'/'restrict'/'volatile'/'_Atomic')* assignment-expression /
      ('const'/'restrict'/'volatile'/'_Atomic')+ 'static' assignment-expression /
      ('const'/'restrict'/'volatile'/'_Atomic')* assignment-expression? /
      ('const'/'restrict'/'volatile'/'_Atomic')* '*'
    ) ']' attribute-specifier-sequence? /

    # Function suffix
    '(' parameter-type-list? ')' attribute-specifier-sequence?
  )*

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

-- | Type Parsing System Overview
--
--   This module implements C-style type parsing with these
--   key components:
--
--   1. Token Collection
--     - We collect tokens into bitsets (TSToks) where each
--       bit represents a token
--     - Tokens can appear in any order (e.g., "extern const
--       long" or "long const extern")
--     - Token counts are tracked separately for error reporting
--       (e.g., "int int" is invalid)
--
--   2. Type Construction Process
--     - First phase: Collect all tokens using `SpecQual`
--       transformations
--     - Second phase: Convert tokens to a base type (e.g.,
--       "unsigned long int" → ULong_)
--     - Third phase: Apply decorators using `TSChangeType`
--       transformations
--
--   3. Composition Order
--     - Type transformations use `Dual (Endo Type)` to apply in
--       reverse parsing order
--     - This handles C's inside-out declarations like "int
--       *(*foo[10])(void)"
--     - The innermost type (int) is modified by each operation
--       working outward

-- Type specifier token - used to count occurrences in parsing
data TSTok
  = -- Type specifiers

    -- | @void@
    TStVoid
  | -- | @char@
    TStChar
  | -- | @short@
    TStShort
  | -- | @int@
    TStInt
  | -- | @long@
    TStLong
  | -- | @float@
    TStFloat
  | -- | @double@
    TStDouble
  | -- | @signed@
    TStSigned
  | -- | @unsigned@
    TStUnsigned
  | -- | @_Bool@
    TStBool
  | -- | @_Complex@
    TStComplex
  | -- | @_Decimal32@
    TStDecimal32
  | -- | @_Decimal64@
    TStDecimal64
  | -- | @_Decimal128@
    TStDecimal128
  | -- | @_BitInt(...)@
    TStBitInt
  | -- Type qualifiers

    -- | @const@
    TStConst
  | -- | @volatile@
    TStVolatile
  | -- | @restrict@
    TStRestrict
  | -- | @\_Atomic@
    TStAtomic
  | -- Storage class specifiers

    -- | @register@
    TStRegister
  | -- | @auto@
    TStAuto
  | -- | @static@
    TStStatic
  | -- | @extern@
    TStExtern
  | -- | @\_Thread\_local@, thread\_local
    TStThreadLocal
  | -- | @typedef@
    TStTypedef
  | -- | @constexpr@
    TStConstexpr
  | -- Function specifiers

    -- | @inline@
    TStInline
  | -- | @\_Noreturn@
    TStNoreturn
  | -- Compound type specifiers

    -- | @struct@ ...
    TStStruct
  | -- | @union@ ...
    TStUnion
  | -- | @enum@ ...
    TStEnum
  | -- | \<identifier\>
    -- Alignment specifier
    TStTypedefName
  | -- | @\_Alignas(...)@, alignas(...)
    -- @typeof@ specifiers
    TStAlignas
  | -- | @typeof@
    TStTypeof
  | -- | @typeof\_unqual@
    TStTypeofUnqual
  deriving (Eq, Enum, Show, Generic)
  deriving anyclass (Hashable)

-- Bitset for type specifier tokens
newtype TSToks = TSToks {untstoks :: Word64}
  deriving newtype (Eq, Num, Bits, FiniteBits)

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
          ifonly tst_bitint "_BitInt(...)",
          -- Compound type specifiers
          ifonly tst_struct "struct ...",
          ifonly tst_union "union ...",
          ifonly tst_enum "enum ...",
          ifonly tst_typedefname "<typedef-name>",
          -- Alignment specifiers
          ifonly tst_alignas "alignas(...)"
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

-- compound type specifiers

tst_struct, tst_union, tst_enum, tst_typedefname :: TSToks
tst_struct = TSToks (1 .<<. 29)
tst_union = TSToks (1 .<<. 30)
tst_enum = TSToks (1 .<<. 31)
tst_typedefname = TSToks (1 .<<. 32)

-- alignment specifiers

tst_alignas :: TSToks
tst_alignas = TSToks (1 .<<. 33)

-- typeof specifiers

tst_typeof, tst_typeof_unqual :: TSToks
tst_typeof = TSToks (1 .<<. 34)
tst_typeof_unqual = TSToks (1 .<<. 35)

-- | Content behind an @alignas@ specification.
data AlignAsSpec
  = -- | No `alignas` directive
    AASNone
  | -- | Align as &lt;@type-name@&gt;
    AASTypeName Type
  | -- | Align as &lt;@constant-expression@&gt;
    AASConstExpr Expr
  deriving (Eq, Show)

-- | Content behind an @typeof@ or @typeof\_unqual@ specification.
data TypeOfSpec
  = -- | No typeof-specifier
    TOSNone
  | -- | Use type
    TOSTypeName Type
  | -- | From expression, compute type. Use type.
    TOSExpr Expr
  deriving (Eq, Show)

-- | Container for holding all information collected during type parsing
data TypeTokens = TypeTokens
  { -- | Count of each token type.
    -- This is for error reporting
    -- if user uses too many specifiers and qualifiers.
    _tt_counts :: !(HashMap TSTok Int),
    -- | The real deal.
    _tt_mask :: !TSToks,
    -- | Bit width for @\_BitInt@, if any (/= 0), or none (== 0).
    _tt_bitintwidth :: !Int,
    -- | Specifications for alignment, if any given.
    _tt_alignspec :: !AlignAsSpec,
    -- | Specifications for @typeof@ or @typeof\_unqual@.
    _tt_typeofspec :: !TypeOfSpec,
    -- | Possibly, a single unqualified type under @\_Atomic@.
    _tt_atomictype :: !(Maybe Type),
    -- | Possibly, a type definition
    _tt_typedef :: !(Maybe Type),
    -- | Actual record
    _tt_recorddef :: !(Maybe Record)
  }
  deriving (Eq, Show)

makeLenses ''TypeTokens

-- | Throw an error citing an unknown combination of type tokens.
_unktyptokcombo :: String -> TSToks -> Parser a
_unktyptokcombo name v =
  err
    $ BasicError
    $ printf
      "%sunknown type token combination %s"
      (if null name then "" else name ++ ": ")
      (show v)

-- if type found, find base type. otherwise, report error.
tt2basic :: TSToks -> Parser BaseType
tt2basic v =
  -- we shouldn't bother with other specifiers.
  fmap BTPrim case ((1 .<<. 16) - 1) .&. v of
    -- void
    0x0001 -> pure PTVoid
    -- char variants
    0x0002 -> pure Char_ -- char
    0x0102 -> pure SChar_ -- signed char
    0x0202 -> pure UChar_ -- unsigned char
    -- int variants (0x8)
    0x0008 -> pure Int_ -- (sim.)
    0x0108 -> pure Int_
    0x0208 -> pure UInt_
    -- short variants
    0x0004 -> pure Short_ -- short
    0x000c -> pure Short_ -- short int
    0x0104 -> pure Short_ -- signed short
    0x010c -> pure Short_ -- signed short int
    0x0204 -> pure UShort_ -- (sim.)
    0x020c -> pure UShort_ -- (sim.)
    -- long variants
    0x0010 -> pure Long_ -- long
    0x0018 -> pure Long_ -- long int
    0x0110 -> pure Long_ -- (etc.)
    0x0118 -> pure Long_
    0x0210 -> pure ULong_
    0x0218 -> pure ULong_
    -- long long variants
    0x0030 -> pure LongLong_ -- (sim.)
    0x0038 -> pure LongLong_
    0x0130 -> pure LongLong_
    0x0138 -> pure LongLong_
    0x0230 -> pure ULongLong_
    0x0238 -> pure ULongLong_
    -- floating point types
    0x0040 -> pure Float_
    0x0080 -> pure Double_
    0x0090 -> pure LongDouble_
    -- complex types
    0x0840 -> pure ComplexFloat_
    0x0880 -> pure ComplexDouble_
    0x0890 -> pure ComplexLongDouble_
    -- bool
    0x0400 -> pure Bool_
    -- decimal types
    0x1000 -> pure Decimal32_
    0x2000 -> pure Decimal64_
    0x4000 -> pure Decimal128_
    -- bit int types
    0x8000 -> pure $ BitInt_ 0
    0x8100 -> pure $ BitInt_ 0
    0x8200 -> pure $ BitInt_ 0
    -- error case
    _ -> _unktyptokcombo "tt2basic" v

-- | decorate types ... why 'Dual'? because if you have something like:
--
--  @int *(*foo[10])(void)@
--
-- the declarations read inside out. the innermost type (int) is
-- modified by the successive operations, so we need to produce
-- transformations in the opposite order of appearance.
newtype TSChangeType = TSChangeType {apchgtyp :: Type -> Type}
  deriving (Monoid, Semigroup) via (Dual (Endo Type))

tt2storage :: TSToks -> Parser TSChangeType
tt2storage v =
  -- bit range for formal storage class specifiers:
  --  - register, auto, static, extern, thread_local, typedef, constexpr
  fmap (TSChangeType . set ty_storclass) case v .&. 0x07F0_0000 of
    0x0000_0000 -> pure $ sc_none
    0x0010_0000 -> pure $ sc_none
    0x0020_0000 -> pure $ sc_auto
    0x0040_0000 -> pure $ sc_static
    0x0080_0000 -> pure $ sc_extern
    0x0100_0000 -> pure $ sc_threadlocal
    0x0200_0000 -> pure $ sc_typedef
    0x0400_0000 -> pure $ sc_constexpr
    _ -> _unktyptokcombo "tt2storage" v

tt2funcspec :: TSToks -> Parser TSChangeType
tt2funcspec v =
  -- inline and _Noreturn, respectively.
  fmap (TSChangeType . set ty_funcspec) case v .&. 0x1800_0000 of
    0x0000_0000 -> pure $ fs_none
    0x0800_0000 -> pure $ fs_inline
    0x1000_0000 -> pure $ fs_noreturn
    _ -> _unktyptokcombo "tt2funcspec" v

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
           "_Atomic" -> handleatomic
           "register" -> push TStRegister tst_register
           "auto" -> push TStAuto tst_auto
           "static" -> push TStStatic tst_static
           "extern" -> push TStExtern tst_extern
           "_Thread_local" -> push TStThreadLocal tst_threadlocal
           "thread_local" -> push TStThreadLocal tst_threadlocal
           "typedef" -> push TStTypedef tst_typedef
           "constexpr" -> push TStConstexpr tst_constexpr
           "inline" -> push TStInline tst_inline
           "_Noreturn" -> push TStNoreturn tst_noreturn
           "_Alignas" -> handlealignas
           "alignas" -> handlealignas
           "typeof" -> handletypeof False
           "typeof_unqual" -> handletypeof True
         |]
   )
 where
  decimal =
    integer_constant_val >>= \case
      IntegerLiteral n _ -> pure $ fromIntegral n
  prepush token mask =
    over tt_counts (HashMap.insertWith (+) token 1)
      . over tt_mask (<> mask)
  push token mask = pure $ SpecQual $ prepush token mask
  handleatomic =
    choice
      [ lx0 $ inpar $ parsetype <&> \ty ->
          SpecQual (prepush TStAtomic tst_atomic)
            <> SpecQual (set tt_atomictype (Just ty)),
        push TStAtomic tst_atomic
      ]
  handlealignas =
    lx0
      $ inpar
      $ choice
        [ parsetype <&> \ty ->
            SpecQual (prepush TStAlignas tst_alignas)
              <> SpecQual (set tt_alignspec (AASTypeName ty)),
          expr_ <&> \ex ->
            SpecQual (prepush TStAlignas tst_alignas)
              <> SpecQual (set tt_alignspec (AASConstExpr ex))
        ]
  handletypeof unqual =
    let (tok, tokbit)
          | unqual = (TStTypeofUnqual, tst_typeof_unqual)
          | otherwise = (TStTypeof, tst_typeof)
        f = (SpecQual (prepush tok tokbit) <>) . SpecQual . set tt_typeofspec
     in lx0
          $ inpar
          $ choice
            [ parsetype <&> f . TOSTypeName,
              expr_ <&> f . TOSExpr
            ]
  push_bitint width =
    pure
      $ SpecQual (prepush TStBitInt tst_bitint)
      <> SpecQual (set tt_bitintwidth width)
  handlelong = pure $ SpecQual \tt ->
    -- if we pushed it before then we use a different bit.
    case HashMap.lookup TStLong tt._tt_counts of
      Nothing -> prepush TStLong tst_long tt
      Just _ -> prepush TStLong tst_longlong tt

-- parse the 'type-specifier-qualifier+' rule, part of the type-name rule.
--
-- Parse all type specifiers/qualifiers and combine them into a single type
-- with proper storage class, base type, and qualifiers.
typespecquals :: Parser Type
typespecquals = do
  -- Implemented:
  --  - Type specifiers
  --  - Type qualifiers
  -- Not Implemented Yet:
  --  - Compound type specifiers
  --  - Typedef specifier, typedef name
  --  - Alignment specifiers
  --
  -- Build specifier-qualifier transformations (SpecQual);
  -- construct a BaseType (bt1).
  SpecQual f <- chainl (<>) typespecqual typespecqual
  let tt = f $ TypeTokens mempty mempty 0 AASNone TOSNone Nothing Nothing Nothing
  checksanity tt
  bt0 <- tt2basic (_tt_mask tt)
  bt1 <- case tt ^. tt_mask .&. tst_bitint of
    0 -> pure bt0 -- absent _BitInt(N) type specifier.
    _ ->
      let bw = tt ^. tt_bitintwidth
       in if 0 < bw && bw < fromIntegral (maxBound :: Word16)
            then case bt0 of
              BTPrim (PTInt signed (ILBitInt _)) ->
                pure $ BTPrim $ PTInt signed (ILBitInt $ fromIntegral bw)
              _ -> err $ InternalError "typespecquals: bt0 not BTPrim"
            else
              err
                $ BasicError
                  ("typespecquals: bad _BitInt width " ++ show bw)
  -- Promote the BaseType into a Type, representing a fully qualified
  -- C type with storage duration annotations. Decorate this Type.
  let ty0 = basetype2type bt1
  chgtyp0 <- (<>) <$> tt2storage tt._tt_mask <*> tt2funcspec tt._tt_mask
  pure $ apchgtyp chgtyp0 ty0
 where
  badcondition TStLong c = c > 2
  badcondition _ c = c > 1
  checksanity tt = case tt ^. tt_counts of
    cc -> case HashMap.filterWithKey badcondition cc of
      map2
        | HashMap.null map2 -> pure ()
        | otherwise -> err $ BasicError "too many specifiers"

-- | Parse the attribute-specifier-sequence rule
attrspecs :: Parser [Attribute]
attrspecs = lx0 $ indbsqb $ attribute `sepBy` lx0 comma
 where
  attribute = attrtok <*> lx0 (inpar baltoks)
  idstr = byteStringOf identifier
  notdelim = (`notElem` "(){}[]")
  baltoks = byteStringOf $ skipSatisfy notdelim
  attrtok = do
    tok1 <- idstr
    option
      (StandardAttribute tok1)
      (ws0 >> lx0 doublecolon >> PrefixedAttribute tok1 <$> lx1 idstr)

-- | Declaration
data Decl' = Decl' Symbol Type
  deriving (Eq, Show)

-- | In declarator parsing, need identifier to be present or absent?
data DeclMode = RequireIdentifier | RequireNoIdentifier
  deriving (Eq, Show)

declarator, absdeclarator :: Parser (Type -> Decl')

-- | Declarator (requires an identifier)
declarator = commondeclarator RequireIdentifier

-- | Abstract declarator (does not accept an identifier)
absdeclarator = commondeclarator RequireNoIdentifier

-- | Declarator (abstract or not)
--
-- An *abstract* declarator does not accept an identifier, but a
-- (regular) declarator does.
commondeclarator :: DeclMode -> Parser (Type -> Decl')
commondeclarator declmode = do
  sym <- newsymbol
  (Decl' sym .) <$> decl_ sym
 where
  decl_ :: Symbol -> Parser (Type -> Type)
  decl_ sym = do
    ptr <- pointer -- optional pointer chain
    bas <- basedecl sym -- base declarator (identifier/grouped decflarator)
    afu <- arrfuncdecl -- array or function suffixes, if any
    pure $ appEndo . getDual $ mconcat [ptr, bas, afu]
  qualifier =
    $( switch_ws1
         [|
           case _ of
             "const" -> pure tq_const
             "restrict" -> pure tq_restrict
             "volatile" -> pure tq_volatile
             "_Atomic" -> pure tq_atomic
           |]
     )
  qualifiers = chainl (<>) (pure mempty) qualifier
  qualifiers1 = chainl (<>) qualifier qualifier
  attrspecs0 = option mempty attrspecs
  pointer, arrfuncdecl :: Parser (Dual (Endo Type))
  pointer =
    wrap <$> option id do
      lx0 star
      attrs <- attrspecs0
      qual <- qualifiers
      pure $ over ty_base \bt -> BTPointer $ QualifiedType bt qual attrs
  arrfuncdecl = chainl (<>) (pure mempty) (array <|> function)
  array, function :: Parser (Dual (Endo Type))
  array =
    wrap <$> do
      let mkarrtype ty = ArrayType ArraySizeNone ty ASNoStatic mempty
          wraparrtype attrs arrty =
            basetype2type (BTArray arrty) & over ty_attributes (<> attrs)
      f1 <- lx0 $ insqb0 do
        -- parse index part of the declarator.
        --  * 'static' in index:
        -- 'static' as in an array size specifies that the array may not
        -- be NULL and it will contain at least as many items as specified.
        --  * qualifiers and the star (*):
        -- for VLA expressions and such.
        let static = lx1 static' $> set at_static ASStatic
            arqua0 = qualifiers <&> set at_qual
            arqua1 = qualifiers1 <&> set at_qual
            ordinary1 = set at_size . ArraySizeExpr <$> assign
            ordinary0 = option (set at_size ArraySizeNone) ordinary1
            vlastar = lx0 star $> set at_size ArraySizeNone
            three = liftM3 \f g h -> f . g . h
            two = liftM2 (.)
        choice
          [ three static arqua0 ordinary1,
            three arqua1 static ordinary1,
            two arqua0 ordinary0,
            two arqua0 vlastar
          ]
      f2 <- attrspecs0 <&> wraparrtype
      pure $ f2 . f1 . mkarrtype
  function =
    wrap <$> do
      f1 <- lx0 $ inpar do
        (ps, var) <-
          branch
            (lx0 tripledot)
            (pure ([], Variadic))
            ( do
                let paramdecl = do
                      attrs <- option mempty attrspecs0
                      partype1 <- typespecquals
                      dec <- declarator <|> absdeclarator
                      let Decl' sym partype2 = dec partype1
                      let partype3 = set ty_attributes attrs partype2
                      pure $ Param partype3 sym
                ps1 <- paramdecl `sepBy1` lx0 comma
                var1 <-
                  option NotVariadic (lx0 comma >> lx0 tripledot $> Variadic)
                pure (ps1, var1)
            )
        pure \retty -> FuncInfo ps retty var
      f2 <-
        attrspecs0 <&> \attrs ty ->
          basetype2type (BTFunc ty) & over ty_attributes (<> attrs)
      pure $ f2 . f1
  wrap = Dual . Endo
  basedecl :: Symbol -> Parser (Dual (Endo Type))
  basedecl sym =
    wrap <$> do
      lx0 $ inpar (decl_ sym) <|> do
        when (declmode == RequireIdentifier) do
          byteStringOf identifier_def >>= symgivename sym
        qual <- qualifiers
        pure $ over ty_qual (<> qual)

-- | Construct the body of a record type, which may be a struct or a union.
--
-- This parser can parse either a declaration or a definition. It assigns
-- a new symbol and optionally tag.
structorunion_body :: Parser ((Symbol -> RecordInfo -> Record) -> Record)
structorunion_body = do
  withOption
    (byteStringOf identifier_def)
    (\i -> decl i <|> def (Just i))
    (def Nothing)
 where
  decl i = do
    sym <- newsymbol
    symgivename sym i
    pure \con -> con sym RecordDecl
  def maybename = do
    sym <- newsymbol
    maybe (pure ()) (symgivename sym) maybename
    lx0 $ incur do
      attrs <- option [] attrspecs
      typebase <- typespecquals
      let static_assert = do
            lx1 static_assert'
            e <- expr_
            l <- optional (lx0 comma >> lx1 string_literal_val)
            pure . pure $ RecordStaticAssertion $ StaticAssertion e l
          field = do
            let bitfielddecl = do
                  optdecl <- optional declarator
                  Decl' membsym membtype <- case optdecl of
                    Just change -> pure $ change typebase
                    _ -> do
                      membsym' <- newsymbol
                      pure $ Decl' membsym' typebase
                  lx0 colon
                  bitwidth <- optional $ CIEUnresolved <$> expr_
                  pure $ RecordField attrs membtype membsym bitwidth
                normaldecl = do
                  Decl' membsym membtype <- declarator <*> pure typebase
                  pure $ RecordField attrs membtype membsym Nothing
                bitfielddecls = bitfielddecl `sepBy` lx0 comma
                normaldecls = normaldecl `sepBy` lx0 comma
            bitfielddecls <|> normaldecls
      ri <- concat <$> (static_assert <|> field) `sepBy` lx0 semicolon
      pure \con -> con sym (RecordDef ri)

-- | Parse types
parsetype :: Parser Type
parsetype = err $ InternalError "parsetype not implemented yet"
