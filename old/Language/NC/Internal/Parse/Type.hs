{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | Parse type names and declarations.
module Language.NC.Internal.Parse.Type (
  -- * Type name
  typename,

  -- * Attributes
  attrspecs,

  -- * Declaration
  declaration,
  declarator,
  absdeclarator,
  optid_declarator,
  bracedinitializer,
  initializer,
) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Monoid
import Language.NC.Internal.Lex
import {-# SOURCE #-} Language.NC.Internal.Parse.Op
import Language.NC.Internal.Prelude hiding (assign)

-- in this parser-lexer we parse type names.

-- we also define C types.

{- Optimized C23 fragment (in PEG) of the type-name rule for
  typename implementation:

# Top-level type name rule
type-name ←
  # Phase 1: Parse type specifiers and qualifiers
  (type-specifier-qualifier ('[' '[' attribute? (',' attribute?)* ']' ']')?)+

  # Phase 2: Parse optional abstract declarator
  (pointer direct-abstract-declarator? / direct-abstract-declarator)?

# Type specifier or qualifier (consolidated)
type-specifier-qualifier ←
  # Primitive types (ordered for parsing efficiency)
  'void' / 'char' / 'short' / 'int' / 'long' / 'float' / 'double' /
  'signed' / 'unsigned' / '_Bool' / '_Complex' / '_Decimal32' / '_Decimal64' /
  '_Decimal128' / '_BitInt' '(' constant-expression ')' /

  # Type qualifiers (common, parse early)
  'const' / 'restrict' / 'volatile' / '_Atomic' /

  # _Atomic newtype
  '_Atomic' '(' type-name ')' /

  # Compound type
  ('struct' / 'union') ('[' '[' attribute? (',' attribute?)* ']' ']')? (
    identifier? '{' (
      # Member declaration list - inlined
      (('[' '[' attribute? (',' attribute?)* ']' ']')? (type-specifier-qualifier
        ('[' '[' attribute? (',' attribute?)* ']' ']')?)+ (
        # Member declarator list - inlined
        (declarator / (declarator? ':' constant-expression))
        (',' (declarator / (declarator? ':' constant-expression)))*
      )? ';' /
      'static_assert' '(' constant-expression (',' string-literal)? ')' ';'
    )+ '}' /
    identifier
  ) /
  'enum' ('[' '[' attribute? (',' attribute?)* ']' ']')? identifier?
    (':' (type-specifier-qualifier ('[' '[' attribute? (',' attribute?)* ']' ']')?)+ )?
    ('{' (enumeration-constant ('[' '[' attribute? (',' attribute?)* ']' ']')?
      ('=' constant-expression)?)
      (',' (enumeration-constant ('[' '[' attribute? (',' attribute?)* ']' ']')?
        ('=' constant-expression)?))* ','? '}')? /

  # typeof and alignas
  ('typeof' / 'typeof_unqual') '(' (type-name / expression) ')' /
  'alignas' '(' (type-name / constant-expression) ')' /

  # typedef-name. but typedef-name is just an identifier.
  identifier

# Declarator: A declarator that declares an identifier. See abstract declarator.
# See also comments in Language.NC.Internal.Types.Parse (Declarator) for
# the idea of a declarator vs. a declaration. Essentially, in the declaration:
#   int a, b, *c, (*d)[2];
# there are four declarators: [a], [b], [*c], and [(*d)[2]].
declarator ←
  pointer? direct-declarator

# Direct declarator: a declarator, but no pointer chain in the beginning.
direct-declarator ←
  (identifier ('[' '[' attribute? (',' attribute?)* ']' ']')? /
    '(' declarator ')')
  (
    # Array suffix
    '[' (
      'static' type-qualifier* assignment-expression /
      type-qualifier+ 'static' assignment-expression /
      type-qualifier* assignment-expression? /
      type-qualifier* '*'
    ) ']' ('[' '[' attribute? (',' attribute?)* ']' ']')? /

    # Function suffix
    '(' (
      parameter-declaration (',' parameter-declaration)* (',' '...')? /
      '...'
    )? ')' ('[' '[' attribute? (',' attribute?)* ']' ']')?
  )*

# Abstract declarator: is a declarator that does NOT declare an identifier.
abstract-declarator ←
  pointer direct-abstract-declarator? /
  direct-abstract-declarator

# Abstract declarator, but no pointer chain in the beginning.
direct-abstract-declarator ←
  # Parenthesized abstract declarator
  ('(' abstract-declarator ')'
    ('[' '[' attribute? (',' attribute?)* ']' ']')?) /

  # Array form
  (direct-abstract-declarator? '[' (
    'static' type-qualifier* assignment-expression /
    type-qualifier+ 'static' assignment-expression /
    type-qualifier* assignment-expression? /
    type-qualifier* '*'
  ) ']' ('[' '[' attribute? (',' attribute?)* ']' ']')?) /

  # Function form
  (direct-abstract-declarator? '(' (
    parameter-declaration (',' parameter-declaration)* (',' '...')? /
    '...'
  )? ')' ('[' '[' attribute? (',' attribute?)* ']' ']')?)

  # Zero or more suffixes (array or function)
  (
    '[' (
      'static' type-qualifier* assignment-expression /
      type-qualifier+ 'static' assignment-expression /
      type-qualifier* assignment-expression? /
      type-qualifier* '*'
    ) ']' ('[' '[' attribute? (',' attribute?)* ']' ']')? /

    '(' (
      parameter-declaration (',' parameter-declaration)* (',' '...')? /
      '...'
    )? ')' ('[' '[' attribute? (',' attribute?)* ']' ']')?
  )*

parameter-declaration ←
  ('[' '[' attribute? (',' attribute?)* ']' ']')?
  (type-specifier-qualifier ('[' '[' attribute? (',' attribute?)* ']' ']')?)+
  (declarator / abstract-declarator)?

pointer ←
  '*' ('[' '[' attribute? (',' attribute?)* ']' ']')? type-qualifier* /
  '*' ('[' '[' attribute? (',' attribute?)* ']' ']')? type-qualifier* pointer

type-qualifier ← 'const' / 'restrict' / 'volatile' / '_Atomic'

attribute ←
  attribute-token attribute-argument-clause?

attribute-token ←
  identifier /
  identifier '::' identifier

attribute-argument-clause ←
  '(' balanced-token-sequence? ')'

balanced-token-sequence ←
  # any-token-except-brackets: literally any token except parentheses,
  # square brackets, and curly braces.
  ('(' balanced-token-sequence? ')' /
    '[' balanced-token-sequence? ']' /
    '{' balanced-token-sequence? '}' /
    any-token-except-brackets)*

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

Key point: Rather than building a CST and then walking it, transform the Type
node directly as parsing proceeds, from the base type outward.
-}

-- as you can see down here, quite a lot of effort was spent in
-- the permutation parsing of type qualifiers and specifiers.

-- | Permutation parsing system
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
--       "unsigned long int" → PTULong)
--     - Third phase: Apply decorators using `TSChangeType`
--       transformations

-- | Type specifier token - used to count occurrences in parsing
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
  | -- | @\_Atomic(...)@
    TStAtomicNewtype
  deriving (Eq, Enum, Show, Generic)
  deriving anyclass (Hashable)

-- | Bitset for type specifier tokens
newtype TSToks = TSToks Word64
  deriving newtype (Eq, Num, Bits, FiniteBits)
  deriving (Semigroup, Monoid) via (Ior TSToks)

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

tst_primmask :: (Bits a, Num a) => a
tst_primmask = 1 .<<. 16 - 1

-- type qualifiers

tst_const, tst_volatile, tst_restrict, tst_atomic :: TSToks
tst_const = TSToks (1 .<<. 16)
tst_volatile = TSToks (1 .<<. 17)
tst_restrict = TSToks (1 .<<. 18)
tst_atomic = TSToks (1 .<<. 19)

-- storage class specifiers

tst_register, tst_auto, tst_static, tst_extern :: TSToks
tst_register = TSToks (1 .<<. 20)
tst_auto = TSToks (1 .<<. 21)
tst_static = TSToks (1 .<<. 22)
tst_extern = TSToks (1 .<<. 23)

tst_threadlocal, tst_typedef, tst_constexpr :: TSToks
tst_threadlocal = TSToks (1 .<<. 24)
tst_typedef = TSToks (1 .<<. 25)
tst_constexpr = TSToks (1 .<<. 26)

-- function specifiers

tst_inline, tst_noreturn :: TSToks
tst_inline = TSToks (1 .<<. 27)
tst_noreturn = TSToks (1 .<<. 28)

-- compound type specifiers

tst_struct, tst_union, tst_enum, tst_typedefname :: TSToks
tst_struct = TSToks (1 .<<. 29)
tst_union = TSToks (1 .<<. 30)
tst_enum = TSToks (1 .<<. 31)
tst_typedefname = TSToks (1 .<<. 32)

-- alignment specifier

tst_alignas :: TSToks
tst_alignas = TSToks (1 .<<. 33)

-- typeof specifiers

tst_typeof, tst_typeof_unqual :: TSToks
tst_typeof = TSToks (1 .<<. 34)
tst_typeof_unqual = TSToks (1 .<<. 35)

-- _Atomic (...) newtype specifier

tst_atomicnewtype :: TSToks
tst_atomicnewtype = TSToks (1 .<<. 36)

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
    -- | Possibly, a type definition. Used for typedef-name, but not typedef.
    _tt_typedefname :: !(Maybe Type),
    -- | Record or enumueration type.
    _tt_compounddef :: !(Maybe Type)
  }
  deriving (Eq, Show)

-- | Empty type tokens.
tt_0 :: TypeTokens
tt_0 = TypeTokens mempty mempty 0 AASNone TOSNone Nothing Nothing Nothing

makeLenses ''TypeTokens

-- | An adapter required because lack of foresight when drafting this module.
--
-- FIXME: remove this in favor of 'TypeofInfo' from "Parse".
tt_typeofspec2 :: Getter TypeTokens (Maybe TypeofInfo)
tt_typeofspec2 = to \tt -> case tt ^. tt_typeofspec of
  TOSNone -> Nothing
  TOSExpr e -> Just $ TQExpr e
  TOSTypeName t -> Just $ TQType t

-- | Throw an error citing an unknown combination of type tokens.
_unktyptokcombo :: String -> TSToks -> Parser a
_unktyptokcombo name v =
  err
    $ InvalidTokenCombination
      (if null name then "" else name)
      (show v)

-- if type found, extract primitive type. otherwise, report error.
tt2prim :: TSToks -> Parser BaseType
tt2prim v =
  -- we shouldn't bother with other specifiers.
  fmap BTPrim case (tst_primmask .&. v) of
    -- void
    0x0001 -> pure PTVoid
    -- char variants
    0x0002 -> pure PTChar -- char
    0x0102 -> pure PTSChar -- signed char
    0x0202 -> pure PTUChar -- unsigned char
    -- int variants (0x8)
    0x0008 -> pure PTInt -- (sim.)
    0x0108 -> pure PTInt
    0x0208 -> pure PTUInt
    -- short variants
    0x0004 -> pure PTShort -- short
    0x000c -> pure PTShort -- short int
    0x0104 -> pure PTShort -- signed short
    0x010c -> pure PTShort -- signed short int
    0x0204 -> pure PTUShort -- (sim.)
    0x020c -> pure PTUShort -- (sim.)
    -- long variants
    0x0010 -> pure PTLong -- long
    0x0018 -> pure PTLong -- long int
    0x0110 -> pure PTLong -- (etc.)
    0x0118 -> pure PTLong
    0x0210 -> pure PTULong
    0x0218 -> pure PTULong
    -- long long variants
    0x0030 -> pure PTLongLong -- (sim.)
    0x0038 -> pure PTLongLong
    0x0130 -> pure PTLongLong
    0x0138 -> pure PTLongLong
    0x0230 -> pure PTULongLong
    0x0238 -> pure PTULongLong
    -- floating point types
    0x0040 -> pure PTFloat
    0x0080 -> pure PTDouble
    0x0090 -> pure PTLongDouble
    -- complex types
    0x0840 -> pure PTComplexFloat
    0x0880 -> pure PTComplexDouble
    0x0890 -> pure PTComplexLongDouble
    -- bool
    0x0400 -> pure PTBool
    -- decimal types
    0x1000 -> pure PTDecimal32
    0x2000 -> pure PTDecimal64
    0x4000 -> pure PTDecimal128
    -- bit int types
    0x8000 -> pure $ PTBitInt 0
    0x8100 -> pure $ PTBitInt 0
    0x8200 -> pure $ PTBitInt 0
    -- error case
    _ -> _unktyptokcombo "tt2prim" v

-- | decorate types ... why 'Dual'? because if you have something like:
--
--  @int *(*foo[10])(void)@
--
-- the declarations read inside out. the innermost type (int) is
-- modified by the successive operations, so we need to produce
-- transformations in the opposite order of appearance.
newtype TSChangeType = TSChangeType {apchgtyp :: Type -> Type}
  deriving (Monoid, Semigroup) via (Dual (Endo Type))

-- | Compute the type qualifiers (e.g., @const@) to attach.
tt2typequal :: TSToks -> Parser TSChangeType
tt2typequal v = do
  -- four qualifiers: const, volatile, restrict, and _Atomic. any combo.
  let pass mask
        | v .&. mask /= 0 = TSChangeType . over ty_qual . (<>)
        | otherwise = const mempty
  pure
    $ mconcat
      [ pass 0x1_0000 tq_const,
        pass 0x2_0000 tq_volatile,
        pass 0x4_0000 tq_restrict,
        pass 0x8_0000 tq_atomic
      ]

-- | Compute storage class specifier (e.g., @extern@) to attach.
tt2storage :: TSToks -> Parser TSChangeType
tt2storage v =
  -- bit range for formal storage class specifiers:
  --  - register, auto, static, extern, thread_local, typedef, constexpr
  --  - mutually exclusive.
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

-- | Compute function specifiers to attach.
tt2funcspec :: TSToks -> Parser TSChangeType
tt2funcspec v = do
  -- inline and _Noreturn. any combo.
  let pass mask
        | v .&. mask /= 0 = TSChangeType . over ty_funcspec . (<>)
        | otherwise = const mempty
  pure
    $ mconcat
      [ pass 0x0800_0000 fs_inline,
        pass 0x1000_0000 fs_noreturn
      ]

-- | For efficient collection of type specifier tokens.
newtype SpecQual = SpecQual (TypeTokens -> TypeTokens)
  deriving (Monoid, Semigroup) via (Endo TypeTokens)

-- | Parse the "type-specifier-qualifier" rule.
--
-- We are more liberal than the C23 standard in that we also parse
-- storage-class-specifiers as well. This simplifies parsing a lot in different
-- places, but may result in unexpected SCS's showing up here and there.
-- Those will need to be culled at the semantic analysis phase.
typespecqual :: Parser SpecQual
typespecqual = do
  $( switch_ws1
       [|
         case _ of
           "alignas" -> handlealignas
           "_Alignas" -> handlealignas
           "auto" -> push TStAuto tst_auto
           "char" -> push TStChar tst_char
           "const" -> push TStConst tst_const
           "constexpr" -> push TStConstexpr tst_constexpr
           "double" -> push TStDouble tst_double
           "enum" -> handleenum
           "extern" -> push TStExtern tst_extern
           "float" -> push TStFloat tst_float
           "inline" -> push TStInline tst_inline
           "int" -> push TStInt tst_int
           "long" -> handlelong
           "register" -> push TStRegister tst_register
           "restrict" -> push TStRestrict tst_restrict
           "short" -> push TStShort tst_short
           "signed" -> push TStSigned tst_signed
           "static" -> push TStStatic tst_static
           "struct" -> handlerecord TStStruct tst_struct con_struct
           "thread_local" -> push TStThreadLocal tst_threadlocal
           "_Thread_local" -> push TStThreadLocal tst_threadlocal
           "typedef" -> push TStTypedef tst_typedef
           "typeof" -> handletypeof TQQual
           "typeof_unqual" -> handletypeof TQUnqual
           "union" -> handlerecord TStUnion tst_union con_union
           "unsigned" -> push TStUnsigned tst_unsigned
           "void" -> push TStVoid tst_void
           "volatile" -> push TStVolatile tst_volatile
           "_Atomic" ->
             branch_inpar
               handleatomicnewtype -- _Atomic (...); specifier. newtype
               handleatomic -- _Atomic ...; qualifier. same type
           "_BitInt" -> (inpar $ decimal) >>= push_bitint
           "_Bool" -> push TStBool tst_bool
           "_Complex" -> push TStComplex tst_complex
           "_Decimal128" -> push TStDecimal128 tst_decimal128
           "_Decimal32" -> push TStDecimal32 tst_decimal32
           "_Decimal64" -> push TStDecimal64 tst_decimal64
           "_Noreturn" -> push TStNoreturn tst_noreturn
           _ -> handletypedefname
         |]
   )
 where
  -- helpers of helpers
  prepush token mask =
    over tt_counts (HashMap.insertWith (+) token 1)
      . over tt_mask (<> mask)
  push token mask = pure $ SpecQual $ prepush token mask
  decimal =
    integer_constant_val >>= \case
      IntegerLiteral n _ -> pure $ fromIntegral n
  -- parse any subsequent tokens and then update TypeTokens state.
  handleatomic = push TStAtomic tst_atomic
  handleatomicnewtype = do
    t <- typename
    if ty_nontrivialquals t
      then
        err
          $ InvalidAtomicQualifiedType t
      else
        pure
          . mconcat
          . map SpecQual
          $ [ set tt_atomictype $ Just $ basetype2type $ BTAtomic t,
              prepush TStAtomicNewtype tst_atomicnewtype
            ]
  handlealignas =
    inpar
      $ choice
        [ expr_ <&> \ex ->
            SpecQual (prepush TStAlignas tst_alignas)
              <> SpecQual (set tt_alignspec (AASConstExpr ex)),
          typename <&> \ty ->
            SpecQual (prepush TStAlignas tst_alignas)
              <> SpecQual (set tt_alignspec (AASTypeName ty))
        ]
  handletypeof qualification =
    let (tok, tokbit)
          | TQUnqual <- qualification = (TStTypeofUnqual, tst_typeof_unqual)
          | otherwise = (TStTypeof, tst_typeof)
        f = (SpecQual (prepush tok tokbit) <>) . SpecQual . set tt_typeofspec
     in inpar
          $ choice
            [ expr_ <&> f . TOSExpr,
              typename <&> f . TOSTypeName
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
  con_struct a b c d = pure $ Record RecordStruct a b c d
  con_union a b c d = pure $ Record RecordUnion a b c d
  handlerecord tok bit con = do
    membernames <- newscope
    sym <- newsymbol
    record <-
      structorunion_body sym membernames
        >>= ($ \a b c d -> basetype2type . BTRecord <$> con a b c d)
    symassoctype sym record
    pure
      . mconcat
      . map SpecQual
      $ [ set tt_compounddef (Just record),
          prepush tok bit
        ]
  handleenum = do
    e <- enum_body <&> basetype2type . BTEnum
    pure
      . mconcat
      . map SpecQual
      $ [ set tt_compounddef (Just e),
          prepush TStEnum tst_enum
        ]
  handletypedefname = do
    ty <- identifier >>= symlookup_mainns >>= symgettype
    pure
      . mconcat
      . map SpecQual
      $ [ set tt_typedefname (Just ty),
          prepush TStTypedefName tst_typedefname
        ]

-- | Do something only if a thing exists (is 'Just').
whenjust :: (Applicative m) => (a -> m ()) -> Maybe a -> m ()
whenjust = maybe (pure ())

-- parse the 'type-specifier-qualifier+' rule, part of the type-name rule.
--
-- Parse all type specifiers/qualifiers and combine them into a single type
-- with proper storage class, base type, and qualifiers.
typespecquals :: Parser Type
typespecquals = do
  -- Implemented:
  --  - Type specifiers
  --  - Type qualifiers
  --  - Compound type specifiers
  --  - Alignment specifiers
  --  - Typedef specifier
  --  - Typedef name
  --
  -- Build specifier-qualifier transformations (SpecQual).
  -- Then transform empty type (t0) to create type (tt).
  SpecQual f <- chainl (<>) typespecqual typespecqual
  let tt = f tt_0
      tm = tt._tt_mask
  checkcounts tt
  checkexclusivity tm
  ty <-
    if
      | tm .&. tst_primmask /= 0 -> handleprim tt tm
      | tm .&. (tst_struct .|. tst_union .|. tst_enum) /= 0 -> handlecompound tt
      | tm .&. (tst_typeof .|. tst_typeof_unqual) /= 0 -> handletypeof tt
      | tm .&. tst_typedefname /= 0 -> handletypedefname tt
      | tm .&. tst_atomicnewtype /= 0 -> handleatomicnewtype tt
      | otherwise ->
          err
            $ InternalError
              "typespecquals parsed a type specifier string, but it \
              \failed to be categorized into any known category."
  -- Decorate this type (tt) using storage and function specifiers and
  -- type qualifiers.
  let three a b c = a <> b <> c
  chgtyp0 <- liftM3 three (tt2storage tm) (tt2funcspec tm) (tt2typequal tm)
  pure $ apchgtyp chgtyp0 ty
 where
  -- make sure each type specifier or qualifier happens at most once,
  -- except for 'long,' which can appear at most twice.
  checkcounts tt =
    let
      badcondition TStLong c = c > 2
      badcondition _ c = c > 1
     in
      case tt ^. tt_counts of
        cc -> case HashMap.filterWithKey badcondition cc of
          map2
            | HashMap.null map2 -> pure ()
            | otherwise ->
                err
                  $ TooManyTypeSpecifiers (show map2)
  -- check exclusivity. e.g., "struct XXX" and "int" cannot appear together.
  checkexclusivity tt =
    let bad =
          let z (x, y)
                | tt .&. x /= 0 = y
                | otherwise = ""
           in filter (not . null)
                $ map
                  z
                  [ (tst_typedefname, "<typedef-name>"),
                    (tst_struct, "struct"),
                    (tst_union, "union"),
                    (tst_enum, "enum"),
                    (tst_typeof, "typeof"),
                    (tst_typeof_unqual, "typeof_unqual"),
                    (tst_primmask, "<primitive type>"),
                    (tst_atomicnewtype, "_Atomic (<type>)")
                  ]
     in case bad of
          (_ : []) -> pure ()
          _ -> err $ IncompatibleTypeCategories $ unwords bad
  -- handle primitive, non-derived types such as "int," "unsigned long,"
  -- "long double _Complex."
  handleprim tt tm = do
    bt0 <- tt2prim tm
    bt1 <- case tt ^. tt_mask .&. tst_bitint of
      0 -> pure bt0 -- absent _BitInt(N) type specifier.
      _ ->
        let bw = tt ^. tt_bitintwidth
         in if 0 < bw && bw < fromIntegral (maxBound :: BitIntWidth)
              then case bt0 of
                BTPrim p ->
                  let p' = p & pt_setbitwidth .~ fromIntegral bw
                   in pure $ BTPrim p'
                _ -> err $ InternalError "typespecquals: bt0 not BTPrim"
              else
                err
                  $ InvalidBitIntWidth (fromIntegral bw)
    pure $ basetype2type bt1
  -- retrieve a property from TypeTokens or else throw an error.
  handle_generic name ge tt = case tt ^. ge of
    Just x -> pure x
    _ -> err $ InternalError $ name ++ ": unexpected absense"
  -- handle a compound type such as struct, union, or enum.
  handlecompound = handle_generic "handlecompound" tt_compounddef
  -- handle a "typeof"
  handletypeof tt =
    handle_generic "handletypeof" tt_typeofspec2 tt <&> \ti ->
      basetype2type
        if tt ^. tt_mask .&. tst_typeof /= 0
          then BTTypeof TQQual ti
          else BTTypeof TQUnqual ti
  -- handle a typedef-name.
  handletypedefname = handle_generic "handletypedefname" tt_typedefname
  -- handle an _Atomic (...) newtype specifier.
  handleatomicnewtype = handle_generic "handleatomicnewtype" tt_atomictype

-- | Parse the attribute-specifier-sequence? rule, but assuming the
-- double square brackets have been parsed already.
attrspecs :: Parser [Attribute]
attrspecs = chainr (<>) a (pure mempty)
 where
  a = branch_indbsqb (pcut (attribute `sepBy` comma) e) failed
  e = BasicError "this attribute isn't understood"
  attribute = attrtok <*> option mempty (inpar baltoks)
  baltoks = byteStringOf $ skipMany $ skipSatisfy (`notElem` "(){}[]")
  attrtok = do
    tok1 <- identifier
    option
      (StandardAttribute tok1)
      (ws0 >> dbcolon >> PrefixedAttribute tok1 <$> identifier)

-- | In declarator parsing, need identifier to be present or absent?
--
--  - argument: action to parse an identifier and then give name to symbol
--  - ('id'): bind the action if an identifier needs to be parsed
--  - (@'const' ...@): ignore the action if an identifier is unexpected
--  - ('optional_'): tolerate failure if an identifier may or may not exist
type DeclMode = Parser () -> Parser ()

declarator, absdeclarator, optid_declarator :: Symbol -> Parser Declarator

-- | Declarator (requires an identifier)
declarator = commondeclarator id

-- | Abstract declarator (does not accept an identifier)
absdeclarator = commondeclarator (const $ pure ())

-- | Optionally identifying declarator (may or may declare an identifier)
optid_declarator = commondeclarator optional_

-- | Declarator (abstract or not)
--
-- An *abstract* declarator does not accept an identifier, but a
-- (regular) declarator does.
commondeclarator :: DeclMode -> Symbol -> Parser Declarator
commondeclarator declmode sym = do
  ptr <- chainr (<>) pointer (pure mempty) -- optional pointer chain
  bas <- basedecl -- base declarator (identifier/grouped decflarator)
  afu <- arrfuncdecl -- array or function suffixes, if any
  -- why pointer -> array/function -> base?
  -- think about this:
  --  *[2] ...... should parse as array of pointers.
  --       ...... ptr * [2]
  --       ...... bas _ [2]
  --       ...... afu [2] <empty>
  --       ...... ptr -> afu, so array of pointers.
  --  (*)[2] .... should parse as pointer to array.
  --         .... ptr _ (*)[2]
  --         .... bas (*) [2]
  --         .... afu [2] <empty>
  --         .... afu -> bas, so pointer [= base type] to array.
  -- Notice that composition order doesn't exactly correspond to syntax order.
  --
  -- Another example:
  --
  --  int **(*)[2]
  --
  -- ... should parse as: (((int ptr) ptr) 2 arr) ptr
  --
  -- meaning, it designates:
  --  a pointer to
  --    a 2-element array of
  --      pointers to
  --        a pointer to
  --          an int.
  pure $ ptr <> afu <> bas
 where
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
  pointer, arrfuncdecl :: Parser Declarator
  pointer =
    Declarator <$> do
      star
      flip pcut BadPointerSyntax do
        attrs <- attrspecs
        qual <- qualifiers
        -- here, we are careful to qualify the *pointer* itself, not the
        -- type that the pointer refers to.
        pure \ty ->
          set ty_qual qual
            . set ty_attributes attrs
            . set ty_base (BTPointer $ ty ^. ty_qualified)
            $ ty
  arrfuncdecl = chainl (<>) (pure mempty) do
    branch_insqb array function <*> attrspecs
  array, function :: Parser ([Attribute] -> Declarator)
  array = do
    let mkarrtype ty = ArrayType ArraySizeNone ty ASNoStatic mempty
        wraparrtype attrs arrty =
          basetype2type (BTArray arrty) & over ty_attributes (<> attrs)
    f1 <- do
      -- parse index part of the declarator.
      --  * 'static' in index:
      -- 'static' as in an array size specifies that the array may not
      -- be NULL and it will contain at least as many items as specified.
      --  * qualifiers and the star (*):
      -- for VLA expressions and such.
      let static = static' $> set at_static ASStatic
          arqua0 = qualifiers <&> set at_qual
          arqua1 = qualifiers1 <&> set at_qual
          ordinary1 = set at_size . ArraySizeExpr <$> assign
          ordinary0 = option (set at_size ArraySizeNone) ordinary1
          vlastar = star $> set at_size ArraySizeNone
          three = liftM3 \f g h -> f . g . h
          two = liftM2 (.)
      choice
        [ two arqua0 ordinary0,
          three static arqua0 ordinary1,
          two arqua0 vlastar,
          three arqua1 static ordinary1
        ]
    pure \as -> Declarator $ wraparrtype as . f1 . mkarrtype
  function = do
    f <- inpar do
      -- we allocate a new scope (symbol table) for parameter names,
      -- which will be embedded into the FuncInfo we return.
      stab <- newscope
      (ps, var) <-
        branch
          tripledot
          (pure ([], Variadic))
          ( do
              ps1 <- inscope stab $ flip sepBy1 comma do
                attrs <- attrspecs
                partype1 <- typespecquals
                sym2 <- newsymbol
                dec <- optid_declarator sym2
                let partype2 = apdecl dec partype1
                let partype3 = set ty_attributes attrs partype2
                pure $ Param partype3 sym2
              var1 <- option NotVariadic (comma >> tripledot $> Variadic)
              pure (ps1, var1)
          )
      -- spaghetti code begin
      pure \retty ->
        -- we remove the storage class specifiers from the return type so that
        -- something like:
        --  static int hi();
        -- is treated as returning 'int' instead of 'static int' (nonsense).
        -- the above is necessary because of a quirk in how we're parsing the
        -- types, where storage class specifiers are parsed alongside
        -- type qualifiers, meaning they'll be attached to the return type.
        ( retty ^. ty_storclass,
          FuncInfo ps (retty & ty_storclass .~ mempty) var stab
        )
    pure \attributes -> Declarator \retty ->
      let (storclass, functype) = f retty
       in basetype2type (BTFunc functype)
            & over ty_storclass (<> storclass)
            & over ty_attributes (<> attributes)
  -- end spaghetti code (you bet it ended ...)
  basedecl :: Parser Declarator
  basedecl = do
    branch_inpar (commondeclarator declmode sym) do
      -- you see how useful making 'declmode' as a function is
      -- from how it saves a branch.
      declmode $ identifier_def >>= symgivename sym
      qual <- qualifiers
      pure $ Declarator $ over ty_qual (<> qual)

-- | Construct the body of a record type, which may be a struct or a union.
--
-- This parser can parse either a declaration or a definition. It assigns
-- a new symbol and optionally tag.
structorunion_body ::
  Symbol ->
  Scope ->
  Parser
    ( ( Symbol ->
        RecordInfo ->
        [Attribute] ->
        Scope ->
        a
      ) ->
      a
    )
structorunion_body sym tab = do
  as <- attrspecs
  f <-
    withOption
      identifier_def
      (\i -> branch_incur (def $ Just i) (decl i))
      (incur $ def Nothing)
  pure \con -> f con as tab
 where
  -- [struct|union] identifier
  decl i = do
    symgivename sym i
    pure \con tab -> con sym RecordDecl tab
  -- [struct|union] identifier? { ... }
  def maybename = do
    symgivename sym `whenjust` maybename
    inscope tab do
      flip pcut BadStructOrUnionDefinition do
        let sa = pure . RecordStaticAssertion <$> static_assert
            field = do
              attrs <- attrspecs
              typebase <- typespecquals
              membsym <- newsymbol
              withOption
                (declarator membsym)
                ( \dec -> do
                    let membtype = apdecl dec typebase
                    branch
                      colon
                      ( do
                          bitwidth <- optional $ CIEUnresolved <$> expr_
                          pure $ RecordField attrs membtype membsym bitwidth
                      )
                      (pure $ RecordField attrs membtype membsym Nothing)
                )
                ( do
                    membsym <- newsymbol
                    colon
                    bitwidth <- CIEUnresolved <$> expr_
                    pure $ RecordField attrs typebase membsym $ Just bitwidth
                )
                `sepBy` comma
        ri <-
          concat
            <$> many (branch static_assert' sa (field <* semicolon))
        pure \con tab -> con sym (RecordDef ri) tab

-- | Parse a static assertion, assuming @static\_assert@ (or the equivalent
-- @\_Static\_assert@) has already been parsed. Thus, this parser will begin
-- by matching the parentheses as in:
--
-- @
-- static\_assert (1);
-- @
--
-- The parser also requries the semicolon. If it isn't found, it will throw
-- an error.
static_assert :: Parser StaticAssertion
static_assert = do
  s <- inpar do
    e <- CIEUnresolved <$> expr_
    l <- optional (comma >> string_literal_val)
    pure $ StaticAssertion e l
  pcut semicolon (MissingSeparator ";")
  pure s

-- | Parse an @enum@ declaration or definition.
enum_body :: Parser EnumType
enum_body = do
  sym <- newsymbol
  let realenumbody attrs tag membtype = do
        symgivename sym `whenjust` tag
        info <-
          EnumDef <$> flip sepEndBy comma do
            sym <- newsymbol
            name <- identifier_def
            symgivename sym name
            attrs <- attrspecs
            value <- optional do
              equal
              pcut (CIEUnresolved <$> expr_) ExpectedExpression
            pure $ EnumConst sym attrs value
        pure $ EnumType sym info attrs membtype
  withOption
    attrspecs
    ( \attrs -> do
        -- a declaration cannot have attributes according to the C23 standard,
        -- so, since we have attributes here, we have a definition.
        tag <- optional identifier_def
        membtype <- optional (colon >> typespecquals)
        incur $ realenumbody attrs tag membtype
    )
    ( do
        -- no attributes ... so this can be either a definition or declaration.
        tag <- optional identifier_def
        membtype <- optional (colon >> typespecquals)
        branch_incur
          (realenumbody [] tag membtype)
          ( case tag of
              Just tag ->
                symgivename sym tag
                  $> EnumType sym EnumDecl [] membtype
              Nothing -> failed
          )
    )

-- | Parse types
--
-- We are more liberal than the C23 standard in that we also parse
-- storage-class-specifiers as well. This simplifies parsing a lot in different
-- places, but may result in unexpected SCS's showing up here and there.
-- Those will need to be culled at the semantic analysis phase.
typename :: Parser Type
typename = do
  sym <- newsymbol
  typespecquals <**> option id (coerce $ absdeclarator sym)

-- | Parse a braced initializer
bracedinitializer :: Parser Initializer
bracedinitializer =
  InitBraced
    <$> do
      (InitItem <$> designators <*> initializer)
        `sepEndBy` comma
 where
  designators = option [] $ designator `manyTill` equal
  designator = do
    let ardsg = DesignatorIndex . CIEUnresolved <$> expr_
    $( switch_ws0
         [|
           case _ of
             "[" -> ardsg <* rsqb
             "<:" -> ardsg <* rsqb
             "." -> DesignatorMember . DMUnresolved <$> identifier
           |]
     )

-- | Parse an initializer
initializer :: Parser Initializer
initializer = branch_incur bracedinitializer (InitExpr <$> assign)

-- | Parse a declaration.
--
-- (I also think this is a wonderful place to add a synchronization point.
-- I'll think about that soon...)
declaration :: Parser Declaration
declaration = sa_or $ (as_or <* semicolon) <|> (regular <* semicolon)
 where
  sa_or = branch static_assert' (StaticAssertDeclaration <$> static_assert)
  as_or = AttributeDeclaration <$> attrspecs
  decini =
    DeclInit
      <$> (newsymbol >>= declarator)
      <*> optional (equal >> initializer)
  regular = NormalDeclaration <$> typespecquals <*> (decini `sepBy1` comma)
