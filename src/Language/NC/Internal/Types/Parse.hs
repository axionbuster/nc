-- On using StrictData. Theoretical justifications:
--  - FlatParse scans a fully-loaded document in a single strict ByteString.
--    Its combinators too are pretty strict, so it makes sense to use strict
--    data structures.
--  - Symbol resolution can happen during parsing; so most nodes will be needed
--    immediately.
--  - C type checking and analysis typically requires the full AST, and partial
--    evaluation isn't beneficial.
--  - The AST is constructed once during parsing and not changed much after.
{-# LANGUAGE StrictData #-}

-- | This giant module merges the parser definitions, C types, and
-- everything else. Primitive non-derived C types are in
-- 'Language.NC.Internal.Types.PrimTypes'.
module Language.NC.Internal.Types.Parse (
  -- * C Types
  Type (..),
  BaseType (..),
  primtype2type,
  basetype2type,
  ty_attributes,
  ty_storclass,
  ty_qual,
  ty_funcspec,
  ty_base,
  ty_alignment,
  ty_qualified,
  ty_nontrivialquals,
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

  -- * C Statements
  CompoundStatement,
  Statement (..),
  BlockItem (..),
  Label (..),
  JumpGoto (..),
  JumpKind (..),
  ForHeader (..),
  clabel_sym,
  cforh_init,
  cforh_cond,
  cforh_post,

  -- * C Literals
  Lit (..),
  IntegerLiteral (..),
  CharacterLiteral (..),
  StringLiteral (..),

  -- * C Declarations
  Declaration (..),
  Declarator (..),
  DeclInit (..),

  -- * C Initializers
  Designator (..),
  DesignatorMember (..),
  InitItem (..),
  Initializer (..),
  init_designation,
  init_value,
  init_isempty,

  -- * Supplemental information
  ConstIntExpr (..),
  Record (..),
  RecordInfo (..),
  RecordField (..),
  RecordType (..),
  EnumType (..),
  EnumInfo (..),
  EnumConst (..),
  FuncType,
  FuncInfo (..),
  Param (..),
  ArrayType (..),
  ArraySize (..),
  ArraySizeStatic (..),
  QualifiedType (..),
  Variadic (..),
  Attribute (..),
  StaticAssertion (..),
  TypeofQual (..),
  TypeofInfo (..),
  cie_expr,
  recinfo_def,
  attr_clause,
  mkrecord,
  rec_type,
  rec_info,
  rec_sym,
  rec_attrs,
  rec_membernames,
  enum_sym,
  enum_info,
  enum_attrs,
  enum_membertype,
  at_size,
  at_type,
  at_qual,
  at_static,
  fun_rettype,
  fun_pars,
  fun_variadic,
  fun_paramnames,

  -- * Error types
  Error (..),
  AnnotatedError (..),
  RelatedInfo (..),
  Severity (..),
  SeverityPolicy,
  aenew,
  aenewerror,
  aenewwarning,
  defaultsevpolicy,

  -- * Symbol types and functions
  Str2Symbol,
  Symbol,
  SymbolKind (..),
  SymbolTable,
  ScopeStack,
  Scope,
  Str,
  newsymbol,
  newscope,
  enterscope,
  exitscope,
  inscope,
  symgivename,
  symlookup_mainns,
  symgettype,
  symassoctype,
  symassoclabel,

  -- * Parser types and functions
  Parser,
  ParserState (..),
  WithSpan (..),
  IntegerSettings (..),
  CharSettings (..),
  ComplianceSettings,
  Endianness (..),
  pscharset,
  pscompliancesettings,
  pserrors,
  psintset,
  pssevpolicy,
  pssymtab,
  runandgetspan,
  pwithspan,
  pcatch,
  pfinally,
  p_onexception,
  emiterror,
  emitwarning,
  emitdiagnostic,
  ist_preciseposbw,
  ist_precisebw,
  comp_allow_block_shadowing,
  comp_separate_typedef_variable_ns,
  mkstate0,

  -- * Debugging
  traceIO,
  dbg_dumpsyms,
) where

import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy (LazyByteString)
import Data.Functor
import Data.HashTable.IO qualified as H
import Data.Hashable (Hashable (..))
import Data.Int (Int8)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid
import Data.Semigroup
import Data.Sequence (Seq ((:|>)))
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Unique
import Data.Word (Word64)
import Debug.Trace qualified as DT
import FlatParse.Stateful (
  ParserIO,
  Span (..),
  ask,
  err,
  failed,
  getPos,
  withError,
  withSpan,
 )
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
    ExprCompoundLiteral Type Initializer
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

-- | A declarator is what transforms a type.
--
-- Basically, a C declaration will have one or more declarators followed by
-- a base type:
--
-- @
-- static long _Complex double a, *b(void), (*c)[2];
-- @
--
-- In the example above, the base type given is @static long \_Complex double@,
-- and there are three declarators:
--
-- - @a@
-- - @*b(void)@
-- - @(*c)[2]@
--
-- Now, there are two types of declarators. (Regular) declarators give an
-- identifier (like @a@, @b@, and @c@ in the example above), while *abstract*
-- declarators do not (e.g., @(\*)(int, long a)@, which does not give an
-- identifier to the function pointer).
--
-- So in the example above, to determine the type of @a@, we need to apply
-- no transformation ('id') to the base type. To determine the type of @b@,
-- we must apply the transformation to turn it into a function that returns
-- a pointer to the base type (@return-type-of-void-function@ '.' @pointer@).
-- For @c@, we need make it a pointer to the array of length 2 (which is
-- because the parentheses around the pointer (@\*@) prioritizes the pointer
-- declaration). It is for this reason a declarator is represented as
-- a function.
newtype Declarator = Declarator {apdecl :: Type -> Type}
  deriving (Semigroup, Monoid) via (Dual (Endo Type))

-- | A designator for an initializer (array index or member access)
data Designator
  = -- | Array index designator: @[constant-expression]@
    DesignatorIndex ConstIntExpr
  | -- | Member designator: @.identifier@
    DesignatorMember DesignatorMember
  deriving (Eq, Show)

-- | Member designator (resolved or unresolved identifier).
data DesignatorMember
  = -- | Symbol is unresolved. Identifier is stored.
    DMUnresolved !Str
  | -- | Symbol has been resolved. It stores the symbol.
    DMResolved !Str !Symbol
  deriving (Eq, Show)

-- | A single item in an initializer list
data InitItem = InitItem
  { -- | Optional designation
    _init_designation :: [Designator],
    -- | The initializer value
    _init_value :: Initializer
  }
  deriving (Eq, Show)

-- | An initializer expression
data Initializer
  = -- | Assignment expression initializer
    InitExpr Expr
  | -- | Braced initializer list (possibly empty)
    InitBraced [InitItem]
  deriving (Eq, Show)

-- | A declarator or a pair of a declarator and an initializer.
data DeclInit
  = -- | The parser introduces identifiers into scope while parsing,
    -- so identifiers are not listed in this definition.
    DeclInit Declarator (Maybe Initializer)

-- | A declaration or definition, including objects, arrays, functions, and
-- typedefs. This encompasses top-level and block definitions.
--
-- A 'Declaration' can be viewed as the negative counterpart to a 'Statement'.
data Declaration
  = -- | Define identifiers, their types, and any initialization.
    -- Attributes can be found inside the 'Type' field.
    --
    -- To properly construct the type for an identifier, it's necessary to
    -- apply the 'Declarator' for each 'DeclInit' in the list. The 'Type'
    -- given under 'NormalDeclaration' only gives the base type. For function
    -- pointer declarations, it only gives the return type.
    --
    -- NOTE: function definitions are not given in 'NormalDeclaration'. Those
    -- go to 'FunctionDefinition'
    NormalDeclaration Type [DeclInit]
  | -- | Define a single function.
    FunctionDefinition Type CompoundStatement
  | -- | Encapsulate a static assertion.
    StaticAssertDeclaration StaticAssertion
  | -- | A declaration made purely of attributes.
    AttributeDeclaration [Attribute]
  deriving (Show)

-- | A finger tree ('Seq') is used to organize statements for fast enough
-- all-around performance for traversal, random access, merger, and manipulation
type CompoundStatement = Seq Statement

-- | A C statement.
--
-- A 'Statement' can be viewed as the positive counterpart to a 'Declaration'.
data Statement
  = -- | A labeled statement (@identifier:@, @case@, or @default@)
    StmtLabeled Label Statement
  | -- | An expression statement (possibly with attributes)
    StmtExpr [Attribute] (Maybe Expr)
  | -- | A compound statement (block)
    StmtCompound (Seq BlockItem)
  | -- | An if statement
    StmtIf Expr Statement (Maybe Statement)
  | -- | A switch statement
    StmtSwitch Expr Statement
  | -- | A while loop
    StmtWhile Expr Statement
  | -- | A do-while loop
    StmtDoWhile Statement Expr
  | -- | A for loop
    StmtFor ForHeader Statement
  | -- | A jump statement (@goto@, @continue@, @break@, @return@)
    StmtJump JumpKind
  | -- | A standalone attribute declaration
    StmtAttribute [Attribute]
  deriving (Show)

-- | A block item (part of a compound statement)
data BlockItem
  = -- | Declaration in a block
    BIDecl Declaration
  | -- | Statement in a block
    BIStmt Statement
  | -- | Label alone in a block
    BILabel Label
  deriving (Show)

-- | Label types. We attach a 'Symbol' to all subtypes so that
-- all labels can be treated uniformly if the situation calls for it.
data Label
  = -- | Named label (@identifier:@)
    LabelNamed [Attribute] Symbol
  | -- | Case label (@case expr:@)
    LabelCase [Attribute] ConstIntExpr Symbol
  | -- | Default label (@default:@)
    LabelDefault [Attribute] Symbol
  deriving (Eq, Show)

-- | A destination for a @goto@ statement.
data JumpGoto
  = JGUnresolved Str
  | JGResolved Symbol
  deriving (Eq, Show)

-- | Jump statement types
data JumpKind
  = -- | @goto identifier;@
    JumpGoto JumpGoto
  | -- | @continue;@
    JumpContinue
  | -- | @break;@
    JumpBreak
  | -- | @return expr?;@
    JumpReturn (Maybe Expr)
  deriving (Eq, Show)

-- | For loop header components
data ForHeader
  = -- | Traditional for(expr?; expr?; expr?)
    ForExpr (Maybe Expr) (Maybe Expr) (Maybe Expr)
  | -- | C99 for(declaration expr?; expr?)
    ForDecl Declaration (Maybe Expr) (Maybe Expr)
  deriving (Show)

-- | Source storage class monoid.
newtype StorageClass = StorageClass {unstorclass :: Int8}
  deriving (Eq, Num, Bits, FiniteBits)
  deriving (Monoid, Semigroup) via (Ior StorageClass)

-- | Source function specifier monoid.
newtype FuncSpec = FuncSpec {unfuncspec :: Int8}
  deriving (Eq, Num, Bits, FiniteBits)
  deriving (Monoid, Semigroup) via (Ior FuncSpec)

-- | An abstract, unique symbol.
newtype Symbol = Symbol Unique
  deriving (Eq)

-- | Source alignment specifier
data Alignment
  = AlignNone
  | -- | @\_Alignas@ with constant expression
    AlignAs ConstIntExpr
  | -- | @\_Alignas@ with type
    AlignAsType Type
  deriving (Eq, Show)

type FuncType = FuncInfo

-- | Is it @typeof@ or @typeof_unqual@?
data TypeofQual = TQQual | TQUnqual
  deriving (Eq, Show)

-- | Is the @typeof@ about a type or an expression?
data TypeofInfo
  = -- | @typeof@ or @typeof\_unqual@ is designates an existing type.
    -- It may store a qualified type even under @typeof\_unqual@.
    TQType Type
  | -- | @typeof@ or @typeof\_unqual@ is about an unevaluated expression.
    TQExpr Expr
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
  | -- | An atomic type specifier (@_Atomic(...)@). The type is not allowed
    -- to be atomic or cvr-qualified.
    BTAtomic Type
  | -- | @typeof@ or @typeof_unqual@
    BTTypeof TypeofQual TypeofInfo
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

-- | Is this record a @union@ or a @struct@?
data RecordType
  = RecordStruct
  | RecordUnion
  deriving (Eq, Show)

-- | A @struct@ or @union@, with body or without.
data Record
  = Record
  { -- | @union@ or @struct@
    _rec_type :: RecordType,
    -- | Symbol for this record
    _rec_sym :: Symbol,
    -- | Declaration or definition
    _rec_info :: RecordInfo,
    -- | Any attributes
    _rec_attrs :: [Attribute],
    -- | Member name to symbol
    _rec_membernames :: Scope
  }
  deriving (Show)

-- | Record information
data RecordInfo = RecordDef [RecordField] | RecordDecl
  deriving (Eq, Show)

-- | Constant integer expression
data ConstIntExpr
  = CIEUnresolved Expr
  | CIEResolved !Int Expr
  deriving (Eq, Show)

-- | Record field
data RecordField
  = -- | Optional attributes; type, symbol, optional bit width.
    RecordField [Attribute] Type Symbol (Maybe ConstIntExpr)
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

-- | Array type, annotated with various information such as size and
-- element type and more.
data ArrayType
  = -- | An array type.
    ArrayType
    { -- | Size
      _at_size :: ArraySize,
      -- | Element type
      _at_type :: Type,
      -- | Has @static@ annotation (parameter position only)
      _at_static :: ArraySizeStatic,
      -- | Any qualifiers (parameter position only)
      _at_qual :: TypeQual
    }
  deriving (Eq, Show)

data ArraySizeStatic
  = ASNoStatic
  | ASStatic
  deriving (Eq, Show)

data ArraySize
  = -- | A fixed size array.
    ArraySize Int
  | -- | Array size given by an expression. Not necessarily variably-sized.
    ArraySizeExpr Expr
  | -- | A flexible array member or unknown size.
    ArraySizeNone
  deriving (Eq, Show)

data StaticAssertion
  = -- | Expression that must not evaluate to 0, and an optional error
    -- message tatat should be emitted.
    StaticAssertion ConstIntExpr (Maybe StringLiteral)
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
    _ty_attributes :: [Attribute],
    _ty_base :: BaseType,
    _ty_qual :: TypeQual,
    _ty_funcspec :: FuncSpec,
    _ty_alignment :: Alignment
  }
  deriving (Eq, Show)

data EnumType
  = -- | An enumeration type.
    EnumType
    { _enum_sym :: Symbol,
      _enum_info :: EnumInfo,
      _enum_attrs :: [Attribute],
      -- | optional member type; 'PTInt' by default.
      _enum_membertype :: Maybe Type
    }
  deriving (Eq, Show)

-- | Enum information
data EnumInfo = EnumDef [EnumConst] | EnumDecl
  deriving (Eq, Show)

-- | Enum constant
data EnumConst
  = -- | Symbol, attributes (if any), explicitly given value (if any).
    EnumConst Symbol [Attribute] (Maybe ConstIntExpr)
  deriving (Eq, Show)

-- | Is a function variadic?
data Variadic = Variadic | NotVariadic
  deriving (Eq, Show)

-- | Function information
data FuncInfo = FuncInfo
  { -- | Named parameters, if any
    _fun_pars :: [Param],
    -- | Return type
    _fun_rettype :: Type,
    -- | Followed by a variadic argument list?
    _fun_variadic :: Variadic,
    -- | Names of parameters, linked to symbols.
    _fun_paramnames :: Scope
  }
  deriving (Show)

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
  = -- | Miscellaneous programming error. Before specific error variants
    -- are added, ad hoc errors go here.
    BasicError String
  | -- | Internal error
    InternalError String
  | -- | Symbol already defined in current scope
    AlreadyDefinedInScope
  | -- | Bad character in a string or character literal.
    BadChar
  | -- | Bad pointer syntax
    BadPointerSyntax
  | -- | Bad struct or union definition
    BadStructOrUnionDefinition
  | -- | Expected an expression
    ExpectedExpression
  | -- | Expected an identifier
    ExpectedIdentifier
  | -- | Expected a literal
    ExpectedLiteral
  | -- | Expected a specific token but didn't find it
    ExpectedToken String
  | -- | Incompatible type categories
    IncompatibleTypeCategories String
  | -- | Incompatible type combination (e.g. \"mixing void with float\")
    --
    --     If the list of \"other types\" is empty, then say,
    --     \"mixing [void] with other types\" (replace [void] with the type).
    IncompatibleTypes String [String]
  | -- | Incorrect integer suffix combo.
    IncorrectIntSuffix
  | -- | Invalid atomic type contains qualified types
    InvalidAtomicQualifiedType Type
  | -- | Invalid _BitInt width
    InvalidBitIntWidth Integer
  | -- | Invalid _BitInt width due to overflow or bad formatting
    InvalidBitIntWidthOverflowOrBadFormat
  | -- | Invalid _Decimal bits specification
    InvalidDecimalBits BitIntWidth
  | -- | Constructing a decimal complex type
    InvalidDecimalComplex
  | -- | Invalid expression in a specific context
    InvalidExpression String
  | -- | Giving signedness to non-integral type
    InvalidSignedness String
  | -- | Token combination is invalid or unknown. Procedure name, and then
    -- best reconstructed type name.
    InvalidTokenCombination
      -- | Procedure name
      String
      -- | Best reconstructed type name
      String
  | -- | Empty or unsupported type specification
    InvalidTypeSpec String
  | -- | Literal too large to fit.
    LiteralTooLarge
  | -- | Error while parsing a generic expression
    MalformedGenericExpression
  | -- | Missing comma or other separator
    MissingSeparator String
  | -- | Unexpected abstract declarator in a context requiring an identifier
    MissingRequiredIdentifier
  | -- | Symbol scope violation
    ScopeViolation String
  | -- | Symbol not found during lookup
    SymbolNotFound Str
  | -- | Too many specifiers of the same type (e.g. "multiple sign specifiers")
    TooManyPrimSpecifiers String
  | -- | Too many type specifiers of a specific kind
    TooManyTypeSpecifiers String
  | -- | Type mismatch in redefinition
    TypeMismatch
  | -- | Unexpected end of file
    UnexpectedEOFError
  | -- | Unrecognized or invalid _BitInt width
    UnknownTypedefName Str
  | -- | Unsupported character encoding.
    UnsupportedEncoding
  | -- | Used as the wrong kind (e.g., used a variable as a type)
    WrongSymbolKind SymbolKind SymbolKind
  deriving (Eq)

-- | An error annotated with span, severity, and optional hints.
data AnnotatedError = AnnotatedError
  { aeerr :: Error,
    aespn :: Span,
    aesev :: Severity,
    aerel :: [RelatedInfo]
  }
  deriving (Eq, Show)

-- | Hints and other related info.
data RelatedInfo = RelatedInfo
  { infospan :: Span,
    infomesg :: Text
  }
  deriving (Eq, Show)

-- | Memory used for strings. We take advantage of strict 'ByteString' sharing.
type Str = ByteString

-- | The kind of a symbol.
data SymbolKind
  = -- | Symbol defines a type or typedef.
    SymIsType
  | -- | Symbol defines neither a type nor a typedef.
    SymIsVar
  deriving (Show, Eq)

-- | Fast IO-based hash table for global symbol information
type FastTable k v = H.BasicHashTable k v

-- | An IO-based hash table from identifier to symbol.
type Str2Symbol = FastTable Str Symbol

-- | A scope. We separate names into namespaces.
data Scope = Scope
  { _scope_mainns_str2sym :: Str2Symbol,
    _scope_labns_str2sym :: Str2Symbol
  }

-- | Stack of scopes for maintaining lexical environments.
newtype ScopeStack = ScopeStack [Scope] -- innermost on top

-- | Complete symbol table consisting of global and scoped information
data SymbolTable = SymbolTable
  { -- | Fast global unique -> identifier table
    _symtab_names :: !(FastTable Symbol Str),
    -- | Current scope stack
    _symtab_scopes :: !(IORef ScopeStack),
    -- | Types
    _symtab_types :: !(FastTable Symbol Type),
    -- | Labels
    _symtab_labels :: !(FastTable Symbol Statement)
  }

-- | Bit widths of primitive integers. Pay especially close attention to
-- the bit width of @long@; on Windows, it's generally 32 bits, but on
-- other platforms, it's generally 64 bits.
--
-- FIXME: 'IntegerSettings' is also responsible for floating-point settings.
data IntegerSettings
  = IntegerSettings
  { _ist_charbitwidth :: !BitIntWidth,
    _ist_shortbitwidth :: !BitIntWidth,
    _ist_intbitwidth :: !BitIntWidth,
    _ist_longbitwidth :: !BitIntWidth,
    _ist_longlongbitwidth :: !BitIntWidth,
    -- | is @char@ represented as @signed char@ or @unsigned char@?
    _ist_charissigned :: !Bool,
    _ist_floatbitwidth :: !BitIntWidth,
    _ist_doublebitwidth :: !BitIntWidth,
    _ist_longdoublebitwidth :: !BitIntWidth
  }
  deriving (Eq, Show)

-- | Endianness for multiple purposes (mostly for integers).
data Endianness = LittleEndian | BigEndian
  deriving (Eq, Show)

-- i might consider adding methods (virtual functions) to CharSettings
-- that encode integer character literals into a custom target encoding
-- that isn't necessarily in Unicode.

-- | @char8_t@, @char16_t@, etc. settings.
--
-- For the signedness of @char@, see 'ComplianceSettings'.
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
newtype ComplianceSettings = ComplianceSettings Word64
  deriving newtype (Eq, Bits)
  deriving (Semigroup, Monoid) via (Ior ComplianceSettings)

-- | Parsing state, to include such things as symbol tables.
data ParserState = ParserState
  { -- | List of all messages, not just errors as the name might suggest.
    _pserrors :: IORef (Seq AnnotatedError),
    -- | Settings for integers.
    _psintset :: IntegerSettings,
    -- | Settings for character types and character and string literals.
    _pscharset :: CharSettings,
    -- | Symbol table for name resolution
    _pssymtab :: SymbolTable,
    -- | Compliance settings for language features
    _pscompliancesettings :: ComplianceSettings,
    -- | Function that determines final severity of messages
    _pssevpolicy :: SeverityPolicy
  }

-- | The parser, which lives in IO.
type Parser = ParserIO ParserState Error

-- | Data with span. Bogus span could exist if some construct was
-- created in thin air by the parser. Bogus spans will be 0:0.
data WithSpan a = WithSpan {-# UNPACK #-} !Span a
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
primtype2type pt = Type mempty mempty (BTPrim pt) mempty mempty AlignNone

instance Show Error where
  show = \case
    BasicError s -> s
    InternalError s -> printf "internal error: %s" s
    AlreadyDefinedInScope -> "symbol already defined in current scope"
    BadChar -> "invalid character in a string or character literal"
    BadPointerSyntax -> "bad pointer syntax"
    BadStructOrUnionDefinition -> "bad struct or union definition"
    ExpectedExpression -> "expected an expression"
    ExpectedIdentifier -> "expected an identifier"
    ExpectedLiteral -> "expected a literal"
    ExpectedToken s -> printf "expected token: %s" s
    IncompatibleTypeCategories s -> printf "incompatible type categories: %s" s
    IncompatibleTypes s [] -> printf "mixing %s with other types" s
    IncompatibleTypes s ts -> printf "mixing %s with %s" s (show ts)
    IncorrectIntSuffix -> "incorrect integer suffix"
    InvalidAtomicQualifiedType t -> printf "atomic type cannot contain qualified types: %s" (show t)
    InvalidBitIntWidth w -> printf "invalid _BitInt width %v" w
    InvalidBitIntWidthOverflowOrBadFormat ->
      "invalid _BitInt width due to overflow or bad formatting"
    InvalidDecimalBits b -> printf "invalid _Decimal bits %v" b
    InvalidDecimalComplex -> "constructing a decimal complex type"
    InvalidExpression s -> printf "invalid expression: %s" s
    InvalidSignedness t ->
      printf "giving signedness to non-integral type %s" t
    InvalidTokenCombination name typename ->
      if null name
        then printf "invalid token combination for type %s" typename
        else printf "invalid token combination in %s for type %s" name typename
    InvalidTypeSpec t -> printf "empty or unsupported type specification %s" t
    LiteralTooLarge -> "literal is too large to fit"
    MalformedGenericExpression -> "malformed generic expression"
    MissingRequiredIdentifier -> "missing required identifier"
    MissingSeparator s -> printf "missing separator: %s" s
    ScopeViolation s -> printf "scope violation: %s" s
    SymbolNotFound s -> printf "symbol not found: %s" (C8.unpack s)
    TooManyPrimSpecifiers s -> printf "too many %s specifiers" s
    TooManyTypeSpecifiers s -> printf "too many type specifiers: %s" s
    TypeMismatch -> "type mismatch in redefinition"
    UnexpectedEOFError -> "unexpected end of file"
    UnknownTypedefName s -> printf "unknown typedef name: %s" (C8.unpack s)
    UnsupportedEncoding -> "unsupported character encoding"
    WrongSymbolKind expected actual ->
      printf
        "wrong symbol kind: expected %s, got %s"
        (show expected)
        (show actual)

instance Exception Error

instance IsString Error where
  fromString = BasicError

instance Show Scope where
  show _ = "<Scope>"

-- | Lens for the clause part of an 'Attribute'
attr_clause :: Lens' Attribute Str
attr_clause = lens getter setter
 where
  getter (StandardAttribute _ c) = c
  getter (PrefixedAttribute _ _ c) = c
  setter (StandardAttribute n _) c = StandardAttribute n c
  setter (PrefixedAttribute p n _) c = PrefixedAttribute p n c

-- | This only compares the symbols for equality (nominal typing).
-- It does NOT look for compatibility, which is a form of structural typing
-- for across translation units.
instance Eq Record where
  r0 == r1 = r0._rec_sym == r1._rec_sym

-- | The symbol table is ignored because parameter names don't matter at all.
instance Eq FuncInfo where
  FuncInfo p0 r0 v0 _ == FuncInfo p1 r1 v1 _ =
    p0 == p1 && r0 == r1 && v0 == v1

-- | Prism for the body of a record.
recinfo_def :: Prism' RecordInfo [RecordField]
recinfo_def = prism' builder matcher
 where
  builder = RecordDef
  matcher = \case
    RecordDef f -> Just f
    RecordDecl -> Nothing

-- | Lens for the expression part of a constant integer expression.
cie_expr :: Lens' ConstIntExpr Expr
cie_expr = lens getter setter
 where
  getter = \case
    CIEUnresolved e -> e
    CIEResolved _ e -> e
  setter (CIEUnresolved _) e = CIEUnresolved e
  setter (CIEResolved i _) e = CIEResolved i e

-- | Create an empty type that wraps around a base type.
basetype2type :: BaseType -> Type
basetype2type bt = Type mempty mempty bt mempty mempty AlignNone

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
asktab = _pssymtab <$> ask

-- | Associate an existing symbol with a name. Replace the name if needed.
-- Also, associate the name in the current scope stack.
symgivename :: Symbol -> Str -> Parser ()
symgivename sym name = do
  st <- asktab
  __throwable_insert st._symtab_names sym name
  (s :| _) <- symlatestscope "symgivename"
  __throwable_insert s._scope_mainns_str2sym name sym

-- | Do a cascading lookup for a symbol given the name from the current
-- symbol scope. If the symbol cannot be found, fail the parser instead of
-- throwing an error. Note: if the scope doesn't exist at all (empty stack),
-- then it is considered an error, so an error will be thrown.
symlookup_mainns :: Str -> Parser Symbol
symlookup_mainns name = do
  (s :| ss) <- symlatestscope "symlookup_mainns"
  (s : ss) & fix \r -> \case
    [] -> failed
    (t : tt) -> do
      liftIO (H.lookup t._scope_mainns_str2sym name) >>= \case
        Nothing -> r tt
        Just sym -> pure sym

-- | Look up the type for a symbol. If it isn't registered, fail.
symgettype :: Symbol -> Parser Type
symgettype sym = do
  tys <- _symtab_types <$> asktab
  liftIO (H.lookup tys sym) >>= \case
    Nothing -> failed
    Just ty -> pure ty

-- | Insert a symbol into a table. If something is already associated with
-- the key, and symbol shadowing is disallowed, throw an 'InternalError'.
-- The error does not go through IO; it uses 'Parser'\'s internal error
-- handling mechanism. If it doesn't exist, or symbol shadowing is allowed,
-- then associate the key with the new value.
__throwable_insert :: (Hashable a) => FastTable a b -> a -> b -> Parser ()
__throwable_insert tab a b = do
  oopsies <- liftIO $ H.mutate tab a \case
    Just{} -> (Just b, True)
    Nothing -> (Just b, False)
  comp <- _pscompliancesettings <$> ask
  when (oopsies && not (comp ^. comp_allow_block_shadowing)) do
    err AlreadyDefinedInScope

-- | Associate a symbol with a type.
symassoctype :: Symbol -> Type -> Parser ()
symassoctype sym ty = do
  st <- asktab
  __throwable_insert st._symtab_types sym ty

-- | It's like 'symgivename', but it is for labels, not the main namespace
-- (of functions, objects, and typedefs).
symassoclabel :: Label -> Str -> Parser ()
symassoclabel label name = do
  st <- asktab
  let sym = view clabel_sym label
  __throwable_insert st._symtab_names sym name
  (s :| _) <- symlatestscope "symassoclabel"
  __throwable_insert s._scope_labns_str2sym name sym
  error "symassoclabel: not implemented"

-- | Get the latest scope as a list or error out.
symlatestscope :: String -> Parser (NonEmpty Scope)
symlatestscope procname = do
  s <- _symtab_scopes <$> asktab
  readIORef s >>= \case
    ScopeStack [] -> err $ InternalError $ procname ++ ": empty stack"
    ScopeStack (t : ts) -> pure (t :| ts)

-- | Create a brand-new 'Scope'
newscope :: Parser Scope
newscope = liftIO $ Scope <$> H.new <*> H.new

-- | Enter a new scope
enterscope :: Scope -> Parser ()
enterscope nametab = do
  st <- asktab
  modifyIORef' (_symtab_scopes st) \(ScopeStack scopes) ->
    ScopeStack (nametab : scopes)

-- | Exit the current scope
exitscope :: Parser ()
exitscope = do
  st <- asktab
  (_ :| s) <- symlatestscope "exitscope"
  writeIORef st._symtab_scopes (ScopeStack s)

-- | Parse @p@ in the given symbol table and then after @p@ finishes pop it.
inscope :: Scope -> Parser a -> Parser a
inscope nametab p = do
  enterscope nametab
  pfinally p exitscope

-- internal function to get integer settings
ain :: Parser IntegerSettings
ain = _psintset <$> ask

-- | Find the precise bit width needed to represent a positive number.
--
-- Signed integers have one fewer bit than the unsigned counterpart.
--
-- For @char@, it depends on the signedness, which is controlled by
-- 'ComplianceSettings'
ist_preciseposbw :: PrimType -> Parser BitIntWidth
ist_preciseposbw p = case p ^. pt_getinttype of
  Just (ITBitInt; ITBool) -> ist_precisebw p
  Just ITChar ->
    ist_precisebw p >>= \q -> do
      c <- view comp_char_is_signed . _pscompliancesettings <$> ask
      pure
        if c
          then q - 1
          else q
  _ ->
    ist_precisebw p <&> \q -> case p ^. pt_getsign of
      Just Signed -> q - 1
      _ -> q

-- | Recall the exact number of bits needed to represent an integral type.
ist_precisebw :: PrimType -> Parser BitIntWidth
ist_precisebw p = case p ^. pt_getinttype of
  Just it -> q it <$> ain
  _ -> error "ist_precisebw called on non-integral type"
 where
  q ITBool = _ist_charbitwidth
  q ITChar = _ist_charbitwidth
  q ITInt = _ist_intbitwidth
  q ITShort = _ist_shortbitwidth
  q ITLong = _ist_longbitwidth
  q ITLongLong = _ist_longlongbitwidth
  q ITBitInt = case p ^. pt_getbitwidth of
    Just bw -> const bw
    _ -> const undefined

__comp_boollens :: Word64 -> Lens' ComplianceSettings Bool
__comp_boollens bitfield = lens getter setter
 where
  getter (ComplianceSettings cs) = cs .&. bitfield /= 0
  setter (ComplianceSettings cs) = \case
    True -> ComplianceSettings (cs .|. bitfield)
    _ -> ComplianceSettings (cs .&. complement bitfield)

instance Show ComplianceSettings where
  show cs = unwords . filter (not . null) $ settings
   where
    b True = id
    b _ = const ""
    settings =
      [ b (cs ^. comp_allow_block_shadowing) "allow block shadowing",
        b
          (cs ^. comp_separate_typedef_variable_ns)
          "separate typedef and variable namespaces"
      ]

-- | Allow shadowing symbols within block scope.
-- When on, the following code is accepted:
--
-- @
-- int main(int x, char **_y) {
--   int x = 0;
--   {
--      int x = 1;
--      char const *x = "hi";
--   }
-- }
-- @
comp_allow_block_shadowing :: Lens' ComplianceSettings Bool
comp_allow_block_shadowing = __comp_boollens 1

-- | Separate namespaces for typedef-names and regular identifiers.
-- When on, the following code is accepted:
--
-- @
--  typedef int h;
--  h h = 23;
-- @
comp_separate_typedef_variable_ns :: Lens' ComplianceSettings Bool
comp_separate_typedef_variable_ns = __comp_boollens 2

-- | If on, @char@ is backed by @signed char@. Otherwise, it's backed by
-- @unsigned char@. Either way, it remains a distinct type.
comp_char_is_signed :: Lens' ComplianceSettings Bool
comp_char_is_signed = __comp_boollens 4

-- | Default compliance settings.
--
--  - Allow block shadowing: ON
--  - Separate typedef and variable namespaces: ON
defaultcompliancesettings :: ComplianceSettings
defaultcompliancesettings =
  mempty
    & (comp_allow_block_shadowing .~ True)
      . (comp_separate_typedef_variable_ns .~ True)

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
  let cs0 = CharSettings PTUInt PTUChar PTUShort PTUInt LittleEndian
  pure (ParserState e is0 cs0 symtab defaultcompliancesettings defaultsevpolicy)
 where
  newsymtable = liftIO do
    -- create a new symbol table and also the initial (global)
    -- stack scope.
    n <- H.new
    s <- ScopeStack . pure <$> (Scope <$> H.new <*> H.new) >>= newIORef
    t <- H.new
    l <- H.new
    pure $ SymbolTable n s t l

-- | Pure \"parser\" to return a 'WithSpan'.
--
-- Argument order was readjusted to agree with 'withSpan' in "flatparse".
pwithspan :: a -> Span -> Parser (WithSpan a)
pwithspan = (pure .) . flip WithSpan

-- | Parse with @p@; on error, record with span and then handle
-- it with @q@. Usage: @pcatch p q@.
pcatch :: Parser a -> (Error -> Parser a) -> Parser a
pcatch p q = do
  st <- getPos
  let h e = do
        en <- getPos
        es <- _pserrors <$> ask
        modifyIORef es (:|> aenew e (Span st en) SeverityError)
        q e
  withError p h

-- | Parse; regardless of error, do the subsequent action.
pfinally :: Parser a -> Parser b -> Parser a
pfinally p q = do
  a <- p `p_onexception` q
  q $> a

-- | Parse; on error, record error and then rethrow.
p_onexception :: Parser a -> (Parser b) -> Parser a
p_onexception p q = pcatch p (\e -> q >> err e)

-- | Run a parser and return a 'WithSpan'.
runandgetspan :: Parser a -> Parser (WithSpan a)
runandgetspan p = withSpan p pwithspan

-- | Helper for emitting diagnostics with appropriate severity.
emitdiagnostic :: Error -> Span -> Severity -> Parser ()
emitdiagnostic e s defaultsev = do
  policy <- _pssevpolicy <$> ask
  let finalsev = policy e defaultsev
  errs <- _pserrors <$> ask
  liftIO $ modifyIORef' errs (|> aenew e s finalsev)

-- | Emit an error.
emiterror :: Error -> Span -> Parser ()
emiterror e s = emitdiagnostic e s SeverityError

-- | Emit a warning.
emitwarning :: Error -> Span -> Parser ()
emitwarning e s = emitdiagnostic e s SeverityWarning

-- | Is the type atomic or cvr-qualified?
ty_nontrivialquals :: Type -> Bool
ty_nontrivialquals ty = ty._ty_qual /= mempty

-- | Focus on the qualified portion of a full type.
ty_qualified :: Lens' Type QualifiedType
ty_qualified = lens getter setter
 where
  getter Type{_ty_base, _ty_qual, _ty_attributes} =
    QualifiedType
      { qt_base = _ty_base,
        qt_qual = _ty_qual,
        qt_attr = _ty_attributes
      }
  setter t QualifiedType{qt_base, qt_qual, qt_attr} =
    t{_ty_base = qt_base, _ty_qual = qt_qual, _ty_attributes = qt_attr}

-- | Create a new record in IO.
mkrecord :: RecordType -> Symbol -> RecordInfo -> [Attribute] -> Parser Record
mkrecord a b c d = Record a b c d <$> newscope

-- | Lifted 'DT.traceIO'
traceIO :: (MonadIO m) => String -> m ()
traceIO = liftIO . DT.traceIO

-- | Dump the symbol table
dbg_dumpsyms :: Parser ()
dbg_dumpsyms = do
  tab <- asktab
  -- print the scopes...
  let go i projection (sc : scs) = do
        traceIO $ printf "Scope %v:" i
        jj <- newIORef (0 :: Int)
        flip H.mapM_ (projection sc) \(name, sym) -> do
          typ <- H.lookup tab._symtab_types sym
          let typst = maybe "" show typ
          j <- readIORef jj <* modifyIORef' jj succ
          traceIO $ printf "  %v: %s %s" j (C8.unpack name) typst
        traceIO ""
        go (i + 1) projection scs
      go _ _ [] = pure ()
  ScopeStack scopes <- readIORef tab._symtab_scopes
  traceIO "Main namespace:"
  liftIO $ go (0 :: Int) _scope_mainns_str2sym scopes
  liftIO $ go (0 :: Int) _scope_labns_str2sym scopes

-- | A 'Getter' to quickly decide if an initializer is empty (i.e., @{}@).
init_isempty :: Getter Initializer Bool
init_isempty = to \case
  InitBraced [] -> True
  _ -> False

instance Show DeclInit where
  show (DeclInit _ mi) = "DeclInit _ " ++ show mi

-- | Point to the 'Symbol' for a 'Label'.
clabel_sym :: Lens' Label Symbol
clabel_sym = lens g s
 where
  g = \case
    LabelNamed _ y -> y
    LabelCase _ _ y -> y
    LabelDefault _ y -> y
  s (LabelNamed a _) b = LabelNamed a b
  s (LabelCase a b _) c = LabelCase a b c
  s (LabelDefault a _) b = LabelDefault a b

-- | Point to the initializer part of a C @for@ statement. It also allows
-- changing the type of the @for@ statement.
cforh_init :: Lens' ForHeader (Either (Maybe Expr) Declaration)
cforh_init = lens g s
 where
  g (ForExpr a _ _) = Left a
  g (ForDecl a _ _) = Right a
  s (ForExpr _ b c) (Left a) = ForExpr a b c
  s (ForExpr _ b c) (Right a) = ForDecl a b c
  s (ForDecl _ b c) (Left a) = ForExpr a b c
  s (ForDecl _ b c) (Right a) = ForDecl a b c

-- | Point to the condition part of a C @for@ statement
cforh_cond :: Lens' ForHeader (Maybe Expr)
cforh_cond = lens g s
 where
  g (ForExpr _ c _) = c
  g (ForDecl _ c _) = c
  s (ForExpr a _ c) b = ForExpr a b c
  s (ForDecl a _ c) b = ForDecl a b c

-- | Point to the post-action part of a C @for@ statement
cforh_post :: Lens' ForHeader (Maybe Expr)
cforh_post = lens g s
 where
  g (ForExpr _ _ p) = p
  g (ForDecl _ _ p) = p
  s (ForExpr a b _) c = ForExpr a b c
  s (ForDecl a b _) c = ForDecl a b c

makeLenses ''Type

makeLenses ''Record

makeLenses ''ArrayType

makeLenses ''FuncInfo

makeLenses ''EnumType

makeLenses ''InitItem

makeLenses ''ParserState
