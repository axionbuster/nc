-- | This giant module merges the parser definitions, C types, and
-- everything else. Primitive non-derived C types are in
-- 'Language.NC.Internal.Types.PrimTypes'.
module Language.NC.Internal.Types.Parse (
  -- * C Types
  Type (..),
  BaseType (..),
  primtype2type,
  ty_storclass,
  ty_qual,
  ty_funcspec,
  ty_base,
  ty_alignment,
  module Language.NC.Internal.Types.PrimTypes,

  -- * C Type Qualifiers
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

  -- * Alignment
  Alignment (..),

  -- * C Expressions
  Expr (..),
  PrimExpr (..),
  GenAssoc (..),

  -- * C Literals
  Lit (..),
  IntegerLiteral (..),
  CharacterLiteral (..),
  StringLiteral (..),

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
  Attribute (..),
  StaticAssertion (..),
  recinfo_def,
  attr_clause,
  rec_attrs,
  rec_info,
  rec_sym,

  -- * Error types
  Error (..),
  PrimTypeBadWhy (..),
  LiteralBadWhy (..),
  SymbolRedefinitionWhy (..),
  AnnotatedError (..),
  RelatedInfo (..),
  Severity (..),
  SeverityPolicy,
  aenew,
  aenewerror,
  aenewwarning,
  defaultsevpolicy,

  -- * Symbol types and functions
  Symbol (..),
  SymbolKind (..),
  SymbolInfo (..),
  SymbolTable (..),
  ScopeStack (..),
  ScopeInfo (..),
  Str,
  newsymbol,
  enterscope,
  exitscope,
  symgivename,
  symassoctype,

  -- * Parser types and functions
  Parser,
  ParserState (..),
  WithSpan (..),
  IntegerSettings (..),
  CharSettings (..),
  ComplianceSettings (..),
  Endianness (..),
  runandgetspan,
  pwithspan,
  throwbasic,
  emiterror,
  emitwarning,
  emitdiagnostic,
  ist_preciseposbw,
  ist_precisebw,
  int_canrepresent,
  mkstate0,
) where

import Control.Exception
import Control.Lens
import Control.Monad.IO.Class
import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (LazyByteString)
import Data.Functor
import Data.HashTable.IO qualified as H
import Data.Hashable (Hashable (..))
import Data.Int (Int8)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid
import Data.Semigroup
import Data.Sequence (Seq)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Unique
import Data.Word
import FlatParse.Stateful (ParserIO, Span (..), ask, err, withSpan)
import Language.NC.Internal.Types.PrimTypes
import Text.Printf (printf)
import UnliftIO.IORef
import Prelude

-- | Represents a primary expression, which is the most basic form
-- of expression in C.
data PrimExpr
  = -- | An identifier
    PrimId Str
  | -- | A literal value (excluding string literals)
    PrimLit Lit
  | -- | A string literal
    PrimStrLit Str
  | -- | An expression in parentheses. Don't count on the
    -- parentheses being included in the span (or not being
    -- included in the span).
    PrimParen Expr
  | -- | A generic selection expression
    PrimGeneric Expr GenAssoc
  deriving (Eq, Show)

-- | A @Generic\_@ selection expression.
data GenAssoc
  = -- | generic selection. if missing, default case.
    GenAssoc (Maybe Type) Expr
  deriving (Eq, Show)

-- | According to the C standard, an expression is an operator bound
-- to operands, or a primary expression.
data Expr
  = -- | primary expression
    Expr (WithSpan PrimExpr)
  | -- | postfix increment (++)
    ExprPostInc Expr
  | -- | postfix decrement (--)
    ExprPostDec Expr
  | -- | function call
    ExprCall Expr [Expr]
  | -- | array subscript
    ExprArray Expr Expr
  | -- | member access using dot (.)
    ExprMember Expr Symbol
  | -- | member access using arrow (->)
    ExprMemberPtr Expr Symbol
  | -- | compound literal, type and value
    ExprCompoundLiteral Expr Expr
  | -- | generic selection
    ExprGeneric Expr [GenAssoc]
  | -- | prefix increment (++)
    ExprPreInc Expr
  | -- | prefix decrement (--)
    ExprPreDec Expr
  | -- | unary plus (+)
    ExprUnaryPlus Expr
  | -- | unary minus (-)
    ExprUnaryMinus Expr
  | -- | logical not (!)
    ExprNot Expr
  | -- | bitwise not (~)
    ExprBitNot Expr
  | -- | cast, type and value
    ExprCast Type Expr
  | -- | dereference using (*)
    ExprDeref Expr
  | -- | address of using (&)
    ExprAddrOf Expr
  | -- | sizeof operator, either type or value
    ExprSizeOf (Either Type Expr)
  | -- | alignof operator, either type or value
    ExprAlignOf (Either Type Expr)
  | -- | multiplication
    ExprTimes Expr Expr
  | -- | division
    ExprDiv Expr Expr
  | -- | modulus
    ExprMod Expr Expr
  | -- | addition
    ExprPlus Expr Expr
  | -- | subtraction
    ExprMinus Expr Expr
  | -- | left shift
    ExprShiftL Expr Expr
  | -- | right shift
    ExprShiftR Expr Expr
  | -- | less than
    ExprLT Expr Expr
  | -- | greater than
    ExprGT Expr Expr
  | -- | less than or equal to
    ExprLE Expr Expr
  | -- | greater than or equal to
    ExprGE Expr Expr
  | -- | equal to
    ExprEQ Expr Expr
  | -- | not equal to
    ExprNE Expr Expr
  | -- | bitwise and
    ExprBitAnd Expr Expr
  | -- | bitwise xor
    ExprBitXor Expr Expr
  | -- | bitwise or
    ExprBitOr Expr Expr
  | -- | logical and
    ExprAnd Expr Expr
  | -- | logical or
    ExprOr Expr Expr
  | -- | conditional operator (?:)
    ExprConditional Expr Expr Expr
  | -- | assignment operator
    ExprAssign Expr Expr
  | -- | assignment operator (+=)
    ExprAssignPlus Expr Expr
  | -- | assignment operator (-=)
    ExprAssignMinus Expr Expr
  | -- | assignment operator (*=)
    ExprAssignTimes Expr Expr
  | -- | assignment operator (/=)
    ExprAssignDiv Expr Expr
  | -- | assignment operator (%=)
    ExprAssignMod Expr Expr
  | -- | assignment operator (<<=)
    ExprAssignShiftL Expr Expr
  | -- | assignment operator (>>=)
    ExprAssignShiftR Expr Expr
  | -- | assignment operator (&=)
    ExprAssignBitAnd Expr Expr
  | -- | assignment operator (^=)
    ExprAssignBitXor Expr Expr
  | -- | assignment operator (|=)
    ExprAssignBitOr Expr Expr
  | -- | comma operator
    ExprComma Expr Expr
  deriving (Eq, Show)

-- | A C Literal
data Lit
  = -- | Integer literal
    LitInteger IntegerLiteral
  | -- | Character constant
    LitChar CharacterLiteral
  | -- | String literal
    LitString StringLiteral
  deriving (Eq, Show)

data IntegerLiteral
  = IntegerLiteral Integer PrimType
  deriving (Eq, Show)

data CharacterLiteral
  = -- | Literal character or universal character name, encoding choice delayed.
    CharacterLiteral Char PrimType
  | -- | Specified in octal or hexadecimal.
    IntCharacterLiteral Integer PrimType
  deriving (Eq, Show)

-- | A string literal; escape sequences have been interpreted, but
-- a NUL byte has NOT been inserted at the end. The encoding is:
--
--  - UTF-8 for single-byte-character strings
--  - UTF-16 for two-byte-character strings
--  - UTF-32 for four-byte-character strings
data StringLiteral
  = -- | Interpreted value, type of each element.
    StringLiteral LazyByteString !PrimType
  deriving (Show, Eq)

-- | Source storage class monoid.
newtype StorageClass = StorageClass {unstorclass :: Int8}
  deriving (Eq, Num, Bits, FiniteBits)
  deriving (Monoid, Semigroup) via (Ior StorageClass)

-- | Source function specifier monoid.
newtype FuncSpec = FuncSpec {unfuncspec :: Int8}
  deriving (Eq, Num, Bits, FiniteBits)
  deriving (Monoid, Semigroup) via (Ior FuncSpec)

-- | An abstract, unique symbol.
newtype Symbol = Symbol {unsymbol :: Unique}
  deriving (Eq)

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
  | -- | An atomic type specifier (_Atomic(...))
    BTAtomic QualifiedType
  deriving (Eq, Show)

-- | A qualified type.
data QualifiedType
  = QualifiedType
  { -- | Unqualified base type
    qt_base :: BaseType,
    -- | Qualifications, if any
    qt_qual :: TypeQual,
    -- | Attributes, if any
    qt_attr :: [Attribute]
  }
  deriving (Eq, Show)

-- | A @struct@ or @union@, with body or without.
data Record
  = -- | A struct type.
    RecordStruct
      { _rec_sym :: Symbol,
        _rec_info :: RecordInfo,
        _rec_attrs :: [Attribute]
      }
  | -- | A union type.
    RecordUnion
      { _rec_sym :: Symbol,
        _rec_info :: RecordInfo,
        _rec_attrs :: [Attribute]
      }
  deriving (Eq, Show)

-- | Prism for the body of a record.
recinfo_def :: Prism' RecordInfo [RecordField]
recinfo_def = prism' builder matcher
 where
  builder = RecordDef
  matcher = \case
    RecordDef f -> Just f
    RecordDecl -> Nothing

-- | Record information
data RecordInfo = RecordDef [RecordField] | RecordDecl
  deriving (Eq, Show)

-- | Record field
data RecordField
  = -- | Optional attributes; type, symbol, optional bit width.
    RecordField [Attribute] Type Symbol (Maybe Int)
  | -- | static assertion member
    RecordStaticAssertion StaticAssertion
  deriving (Eq, Show)

-- | Attribute
data Attribute
  = -- | unprefixed name; clauses, uninterpreted.
    StandardAttribute Str Str
  | -- | prefix, name, uninterpreted clauses.
    PrefixedAttribute Str Str Str
  deriving (Eq, Show)

-- | Lens for the clause part of an 'Attribute'
attr_clause :: Lens' Attribute Str
attr_clause = lens getter setter
 where
  getter (StandardAttribute _ c) = c
  getter (PrefixedAttribute _ _ c) = c
  setter (StandardAttribute n _) c = StandardAttribute n c
  setter (PrefixedAttribute p n _) c = PrefixedAttribute p n c

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

data StaticAssertion
  = StaticAssertion Expr (Maybe StringLiteral)
  deriving (Eq, Show)

-- | Monoid representing qualifier monoid.
newtype TypeQual = TypeQual {untypequal :: Int8}
  deriving (Eq, Num, Bits, FiniteBits)
  deriving (Monoid, Semigroup) via (Ior TypeQual)

-- | Source types.
--
-- We also include the storage class declaration for convenience in
-- parsing and organization, though it is not part of the type itself.
data Type = Type
  { _ty_storclass :: StorageClass,
    _ty_base :: BaseType,
    _ty_qual :: TypeQual,
    _ty_funcspec :: FuncSpec,
    _ty_alignment :: Alignment
  }
  deriving (Eq, Show)

-- | Enum information
data EnumInfo = EnumDef [EnumConst] | EnumDecl
  deriving (Eq, Show)

-- | Enum constant
data EnumConst = EnumConst Symbol (Maybe Expr)
  deriving (Eq, Show)

-- | Is a function variadic?
data Variadic = Variadic | NotVariadic
  deriving (Eq, Show)

-- | Function information
data FuncInfo = FuncDef [Param] Type Variadic | FuncDecl
  deriving (Eq, Show)

-- | Function parameter
data Param = Param Type Symbol | ParamUnnamed Type
  deriving (Eq, Show)

-- | Severity of a diagnostic message
data Severity
  = -- | Error stops compilation
    SeverityError
  | -- | Warning continues compilation
    SeverityWarning
  | -- | Note is informational only
    SeverityNote
  deriving (Eq, Show)

-- | Function that determines the final severity for a given error
-- and default severity
type SeverityPolicy = Error -> Severity -> Severity

-- | An error, warning, or message of any kind.
--
-- Not all \"errors\" may be errors.
data Error
  = -- | Miscellaneous programming error
    BasicError String
  | -- | Bad primitive type
    PrimTypeBadError PrimTypeBadWhy
  | -- | Literal problem
    LiteralBadError LiteralBadWhy
  | -- | Unexpected end of file
    UnexpectedEOFError
  | -- | Internal error
    InternalError String
  | -- | Symbol redefinition
    SymbolRedefinitionError SymbolRedefinitionWhy
  deriving (Eq, Show)

-- | Reasons for symbol redefinition errors
data SymbolRedefinitionWhy
  = -- | Symbol already defined in current scope
    AlreadyDefinedInScope
  | -- | Type mismatch in redefinition
    TypeMismatch
  deriving (Eq, Show)

-- | Why a primitive type couldn't be parsed, or is incorrect.
data PrimTypeBadWhy
  = -- | Too many specifiers of the same type (e.g. "multiple sign specifiers")
    TooManySpecifiers String
  | -- | Incompatible type combination (e.g. \"mixing void with float\")
    --
    --     If the list of \"other types\" is empty, then say,
    --     \"mixing [void] with other types\" (replace [void] with the type).
    IncompatibleTypes String [String]
  | -- | Unrecognized or invalid _BitInt width
    InvalidBitIntWidth Word16
  | -- | Invalid _BitInt width due to overflow or bad formatting
    InvalidBitIntWidthOverflowOrBadFormat
  | -- | Invalid _Decimal bits specification
    InvalidDecimalBits Word16
  | -- | Giving signedness to non-integral type
    InvalidSignedness String
  | -- | Constructing a decimal complex type
    InvalidDecimalComplex
  | -- | Empty or unsupported type specification
    InvalidTypeSpec String
  deriving (Eq)

data LiteralBadWhy
  = -- | Incorrect integer suffix combo.
    IncorrectIntSuffix
  | -- | Literal too large to fit.
    LiteralTooLarge
  | -- | Unsupported character encoding.
    UnsupportedEncoding
  | -- | Invalid character in a string or character literal.
    BadChar
  deriving (Eq)

-- | An error annotated with span, severity, and optional hints.
data AnnotatedError = AnnotatedError
  { aeerr :: !Error,
    aespn :: !Span,
    aesev :: !Severity,
    aerel :: ![RelatedInfo]
  }
  deriving (Eq, Show)

-- | Hints and other related info.
data RelatedInfo = RelatedInfo
  { infospan :: Span,
    infomesg :: Text
  }
  deriving (Eq, Show)

-- | Memory used for strings. We take advantage of strict 'ByteString' sharing
-- done by "flatparse".
type Str = ByteString

-- | The kind of a symbol.
data SymbolKind
  = -- | The symbol is a typedef of another type.
    SymIsTypedef
  | -- | Symbol defines a type.
    SymIsType
  deriving (Show, Eq)

-- | Information about a symbol.
-- This includes the symbol's name, kind, and location.
data SymbolInfo = SymbolInfo
  { -- | The symbol's name.
    si_name :: !Str,
    -- | The symbol's kind.
    si_kind :: !SymbolKind,
    -- | The symbol's location in the source code.
    --   In rare occasions this may be a bogus location, that is,
    --   0:0, if the symbol was created in thin air for various reasons.
    si_loc :: !Span
  }
  deriving (Eq, Show)

-- | Fast IO-based hash table for global symbol information
type FastTable k v = H.BasicHashTable k v

-- | Information stored about a symbol within a scope
data ScopeInfo = ScopeInfo
  { -- | Symbol's name
    scope_name :: !Str,
    -- | Symbol's kind
    scope_kind :: !SymbolKind
    -- Add other properties as needed
  }

-- | Stack of scopes for maintaining lexical environments
newtype ScopeStack = ScopeStack
  { -- | Chain of scope tables, innermost first
    unscopestack :: [FastTable Str Symbol]
  }

-- | Complete symbol table consisting of global and scoped information
data SymbolTable = SymbolTable
  { -- | Fast global unique -> identifier table
    symtab_names :: !(FastTable Symbol Str),
    -- | Current scope stack
    symtab_scopes :: !(IORef ScopeStack),
    -- | Types
    symtab_types :: !(FastTable Symbol Type)
  }

-- | Bit widths of primitive integers. Pay especially close attention to
-- the bit width of @long@; on Windows, it's generally 32 bits, but on
-- other platforms, it's generally 64 bits.
--
-- FIXME: 'IntegerSettings' is also responsible for floating-point settings.
data IntegerSettings
  = IntegerSettings
  { _ist_charbitwidth :: !Word16,
    _ist_shortbitwidth :: !Word16,
    _ist_intbitwidth :: !Word16,
    _ist_longbitwidth :: !Word16,
    _ist_longlongbitwidth :: !Word16,
    -- | is @char@ represented as @signed char@ or @unsigned char@?
    _ist_charissigned :: !Bool,
    _ist_floatbitwidth :: !Word16,
    _ist_doublebitwidth :: !Word16,
    _ist_longdoublebitwidth :: !Word16
  }
  deriving (Eq, Show)

-- | Endianness for multiple purposes (mostly for integers).
data Endianness = LittleEndian | BigEndian
  deriving (Eq, Show)

-- i might consider adding methods (virtual functions) to CharSettings
-- that encode integer character literals into a custom target encoding
-- that isn't necessarily in Unicode.

-- | Currently, the real type that is equal to @wchar_t@.
data CharSettings = CharSettings
  { -- | We'll make @wchar_t@ unsigned for many reasons.
    cst_wchar_type :: PrimType,
    -- | It's good to make @char8_t@ unsigned.
    cst_char8_type :: PrimType,
    -- | must be unsigned.
    cst_char16_type :: PrimType,
    -- | must be unsigned, too.
    cst_char32_type :: PrimType,
    -- | Separate from integer endianness, we record
    --     the endianness used for multi-byte Unicode encodings.
    cst_char_endian :: Endianness
  }

-- | Compliance settings for controlling parser behavior.
data ComplianceSettings = ComplianceSettings
  { -- | Allow variable shadowing in block scope (non-standard, default on)
    comp_allow_block_shadowing :: !Bool
  }

-- | Parsing state, to include such things as symbol tables.
data ParserState = ParserState
  { -- | List of all messages, not just errors as the name might suggest.
    pserrors :: IORef (Seq AnnotatedError),
    -- | Settings for integers.
    psintset :: IntegerSettings,
    -- | Settings for character types and character and string literals.
    pscharset :: CharSettings,
    -- | Symbol table for name resolution
    pssymtab :: SymbolTable,
    -- | Compliance settings for language features
    pscompliancesettings :: ComplianceSettings,
    -- | Function that determines final severity of messages
    pssevpolicy :: SeverityPolicy
  }

-- | The parser, which lives in IO.
type Parser = ParserIO ParserState Error

-- | Data with span. Bogus span could exist if some construct was
-- created in thin air by the parser. Bogus spans will be 0:0.
data WithSpan a = WithSpan !Span a
  deriving (Eq, Show, Functor)

instance Show Symbol where
  show _ = "<symbol>"

instance Show StorageClass where
  show = \case
    0 -> "none"
    s ->
      intercalate " " $
        filter
          (not . null)
          [ if s .&. sc_register /= 0 then "register" else "",
            if s .&. sc_auto /= 0 then "auto" else "",
            if s .&. sc_static /= 0 then "static" else "",
            if s .&. sc_extern /= 0 then "extern" else "",
            if s .&. sc_threadlocal /= 0 then "_Thread_local" else "",
            if s .&. sc_typedef /= 0 then "typedef" else "",
            if s .&. sc_constexpr /= 0 then "constexpr" else ""
          ]

instance Show FuncSpec where
  show = \case
    0 -> "none"
    f ->
      intercalate " " $
        filter
          (not . null)
          [ if f .&. fs_inline /= 0 then "inline" else "",
            if f .&. fs_noreturn /= 0 then "_Noreturn" else ""
          ]

instance Show TypeQual where
  show = \case
    0 -> "none"
    q ->
      intercalate " " $
        filter
          (not . null)
          [ if q .&. tq_const /= 0 then "const" else "",
            if q .&. tq_volatile /= 0 then "volatile" else "",
            if q .&. tq_restrict /= 0 then "restrict" else "",
            if q .&. tq_atomic /= 0 then "_Atomic" else ""
          ]

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

fs_none, fs_inline, fs_noreturn :: FuncSpec
fs_none = FuncSpec 0
fs_inline = FuncSpec 1
fs_noreturn = FuncSpec 2

tq_none, tq_const, tq_volatile, tq_restrict, tq_atomic :: TypeQual
tq_none = TypeQual 0
tq_const = TypeQual 1
tq_volatile = TypeQual 2
tq_restrict = TypeQual 4
tq_atomic = TypeQual 8

-- | Make a bare-bones 'Type' out of a 'PrimType'.
primtype2type :: PrimType -> Type
primtype2type pt = Type mempty (BTPrim pt) mempty mempty AlignNone

instance Show PrimTypeBadWhy where
  show = \case
    TooManySpecifiers s -> printf "too many %s specifiers" s
    IncompatibleTypes s [] -> printf "mixing %s with other types" s
    IncompatibleTypes s ts -> printf "mixing %s with %s" s (show ts)
    InvalidBitIntWidth w -> printf "invalid _BitInt width %v" w
    InvalidBitIntWidthOverflowOrBadFormat ->
      "invalid _BitInt width due to overflow or bad formatting"
    InvalidDecimalBits b -> printf "invalid _Decimal bits %v" b
    InvalidSignedness t ->
      printf "giving signedness to non-integral type %s" t
    InvalidDecimalComplex -> "constructing a decimal complex type"
    InvalidTypeSpec t -> printf "empty or unsupported type specification %s" t

instance Exception Error

instance IsString Error where
  fromString = BasicError

instance Show LiteralBadWhy where
  show = \case
    IncorrectIntSuffix -> "incorrect integer suffix"
    LiteralTooLarge -> "literal is too large to fit"
    UnsupportedEncoding -> "unsupported character encoding"
    BadChar -> "invalid character in a string or character literal"

-- | Create a new empty annotated error with the given span and severity.
aenew :: Error -> Span -> Severity -> AnnotatedError
aenew e s sev = AnnotatedError e s sev mempty

-- | Create a new error with Error severity.
aenewerror :: Error -> Span -> AnnotatedError
aenewerror e s = aenew e s SeverityError

-- | Create a new warning with Warning severity.
aenewwarning :: Error -> Span -> AnnotatedError
aenewwarning e s = aenew e s SeverityWarning

-- | Default severity policy (keeps original severity).
defaultsevpolicy :: SeverityPolicy
defaultsevpolicy _ s = s

-- Make Symbol hashable for use in hash tables
instance Hashable Symbol where
  hashWithSalt s (Symbol u) = hashWithSalt s (hashUnique u)

-- | Create a new unique symbol.
newsymbol :: Parser Symbol
newsymbol = liftIO $ Symbol <$> newUnique

-- | Ask for the global symbol table.
asktab :: Parser SymbolTable
asktab = pssymtab <$> ask

-- | Associate an existing symbol with a name. Replace the name if needed.
-- Also, associate the name in the current scope stack.
symgivename :: Symbol -> Str -> Parser ()
symgivename sym name = do
  st <- asktab
  liftIO $ H.insert st.symtab_names sym name
  (s :| _) <- symlatestscope "symgivename"
  liftIO $ H.insert s name sym

-- | Associate a symbol with a type.
symassoctype :: Symbol -> Type -> Parser ()
symassoctype sym ty = do
  st <- asktab
  liftIO $ H.insert st.symtab_types sym ty

-- | Get the latest scope as a list or error out.
symlatestscope :: String -> Parser (NonEmpty (FastTable Str Symbol))
symlatestscope procname = do
  s <- symtab_scopes <$> asktab
  readIORef s >>= \case
    ScopeStack [] -> err $ InternalError $ procname ++ ": empty stack"
    ScopeStack (t : ts) -> pure (t :| ts)

-- | Enter a new scope
enterscope :: Parser ()
enterscope = do
  st <- asktab
  newscope <- liftIO H.new
  modifyIORef' (symtab_scopes st) \(ScopeStack scopes) ->
    ScopeStack (newscope : scopes)

-- | Exit the current scope
exitscope :: Parser ()
exitscope = do
  st <- asktab
  (_ :| s) <- symlatestscope "exitscope"
  writeIORef st.symtab_scopes (ScopeStack s)

-- internal function to get integer settings
ain :: Parser IntegerSettings
ain = psintset <$> ask

-- | Find the precise bit width needed to represent a positive number.
--
-- Signed integers have one fewer bit than the unsigned counterpart.
--
-- For @char@, it depends on the signedness.
ist_preciseposbw :: PrimType -> Parser Word16
ist_preciseposbw (PTInt _ (ILBitInt n)) = pure n
ist_preciseposbw (PTInt s a) = do
  x <- ist_pbw' a
  pure $ x - fromIntegral (fromEnum (s == Signed))
 where
  ist_pbw' ILShort = _ist_shortbitwidth <$> ain
  ist_pbw' ILInt = _ist_intbitwidth <$> ain
  ist_pbw' ILLong = _ist_longbitwidth <$> ain
  ist_pbw' ILLongLong = _ist_longlongbitwidth <$> ain
  ist_pbw' _ = error "ist_pbw' on _BitInt(N) ... impossible"
ist_preciseposbw (PTChar (Just Unsigned)) = _ist_charbitwidth <$> ain
ist_preciseposbw (PTChar (Just Signed)) = pred . _ist_charbitwidth <$> ain
ist_preciseposbw (PTChar Nothing) = do
  csg <- _ist_charissigned <$> ain
  if csg
    then pred . _ist_charbitwidth <$> ain
    else _ist_charbitwidth <$> ain
ist_preciseposbw (PTFloat f)
  | FTComplex a <- f = (2 *) <$> ist_preciseposbw (PTFloat $ FTReal a)
  | FTReal RFFloat <- f = _ist_floatbitwidth <$> ain
  | FTReal RFDouble <- f = _ist_doublebitwidth <$> ain
  | FTReal RFLongDouble <- f = _ist_longdoublebitwidth <$> ain
  | FTReal RFDecimal128 <- f = pure 128
  | FTReal RFDecimal32 <- f = pure 32
  | FTReal RFDecimal64 <- f = pure 64
ist_preciseposbw t =
  err $
    InternalError $
      "ist_preciseposbw called on unsupported type "
        ++ show t

-- | Recall the exact number of bits needed to represent a scalar type.
ist_precisebw :: PrimType -> Parser Word16
ist_precisebw (PTInt _ a) = case a of
  ILShort -> _ist_shortbitwidth <$> ain
  ILInt -> _ist_intbitwidth <$> ain
  ILLong -> _ist_longbitwidth <$> ain
  ILLongLong -> _ist_longlongbitwidth <$> ain
  ILBitInt n -> pure n
ist_precisebw (PTChar _) = _ist_charbitwidth <$> ain
ist_precisebw t = ist_preciseposbw t

-- | Default compliance settings.
defaultcompliancesettings :: ComplianceSettings
defaultcompliancesettings =
  ComplianceSettings
    { comp_allow_block_shadowing = True
    }

-- | See if an integer constant can be represented by a given type.
int_canrepresent :: Integer -> PrimType -> Parser Bool
int_canrepresent i = \case
  PTBool -> pure $ i == 0 || i == 1
  t@(PTInt Signed _; PTChar (Just Signed)) -> rep Signed t
  t@(PTInt Unsigned _; PTChar (Just Unsigned)) -> rep Unsigned t
  t@(PTChar Nothing) -> do
    s <- _ist_charissigned <$> ain
    rep (if s then Signed else Unsigned) t
  _ -> err (InternalError "int_canrepresent called on a non-integral type")
 where
  rep Signed t = do
    bw <- ist_precisebw t
    let rngtop = 2 ^ (bw - 1) - 1
    let rngbot = negate $ 2 ^ (bw - 1)
    pure $ rngbot <= i && i <= rngtop
  rep Unsigned t = do
    bw <- ist_precisebw t
    pure $ i <= 2 ^ bw

-- | Create a starter state.
--
-- Defaults: these defaults may work better on a UNIX-like system.
-- They definitely don't work on Windows.
--
-- - @long@ is assigned 64 bits as a temporary measure.
-- - @char@ is backed by @signed char@.
-- - @wchar_t@ is currently represented by @int@.
-- - Multi-byte characters are encoded in little-endian Unicode.
-- - Allows block variable shadowing (non-standard).
-- - Uses default severity policy for errors and warnings.
mkstate0 :: IO ParserState
mkstate0 = do
  e <- newIORef mempty
  symtab <- newsymtable
  let is0 = IntegerSettings 8 16 32 64 64 True 32 64 64
  let cs0 = CharSettings UInt_ UChar_ UShort_ UInt_ LittleEndian
  pure (ParserState e is0 cs0 symtab defaultcompliancesettings defaultsevpolicy)
 where
  newsymtable = liftIO do
    -- create a new symbol table and also the initial (global)
    -- stack scope.
    n <- H.new
    s <- ScopeStack . pure <$> H.new >>= newIORef
    t <- H.new
    pure $ SymbolTable n s t

-- | Pure \"parser\" to return a 'WithSpan'.
--
-- Argument order was readjusted to agree with 'withSpan' in "flatparse".
pwithspan :: a -> Span -> Parser (WithSpan a)
pwithspan = (pure .) . flip WithSpan

-- | Run a parser and return a 'WithSpan'.
runandgetspan :: Parser a -> Parser (WithSpan a)
runandgetspan p = withSpan p pwithspan

-- | Throw a 'BasicError'.
throwbasic :: String -> Parser a
throwbasic = err . BasicError

-- | Helper for emitting diagnostics with appropriate severity.
emitdiagnostic :: Error -> Span -> Severity -> Parser ()
emitdiagnostic e s defaultsev = do
  policy <- pssevpolicy <$> ask
  let finalsev = policy e defaultsev
  errs <- pserrors <$> ask
  liftIO $ modifyIORef' errs (|> aenew e s finalsev)

-- | Emit an error.
emiterror :: Error -> Span -> Parser ()
emiterror e s = emitdiagnostic e s SeverityError

-- | Emit a warning.
emitwarning :: Error -> Span -> Parser ()
emitwarning e s = emitdiagnostic e s SeverityWarning

makeLenses ''Type

makeLenses ''Record
