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

  -- * C Literals
  Lit (..),
  IntegerLiteral (..),
  CharacterLiteral (..),
  StringLiteral (..),

  -- * C Declarations.
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
  Str2Symbol,
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

  -- * Error types
  Error (..),
  PrimTypeBadWhy (..),
  LiteralBadWhy (..),
  SymbolError (..),
  TypeParseError (..),
  ExprParseError (..),
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
  inscope,
  symgivename,
  symlookup,
  symgettype,
  symassoctype,

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
  int_canrepresent,
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

-- | A C statement
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

-- | Label types
data Label
  = -- | Named label (@identifier:@)
    LabelNamed [Attribute] Symbol
  | -- | Case label (@case expr:@)
    LabelCase [Attribute] ConstIntExpr  
  | -- | Default label (@default:@)
    LabelDefault [Attribute]
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
newtype Symbol = Symbol {unsymbol :: Unique}
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
  | -- | An atomic type specifier (_Atomic(...)). The type is not allowed
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

-- | An IO-based hash table from identifier to symbol.
type Str2Symbol = FastTable Str Symbol

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
    _rec_membernames :: FastTable Str Symbol
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
      -- | optional member type; 'Int_' by default.
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
    _fun_variadic :: Variadic
  }
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
  = -- | Miscellaneous programming error. Before specific error variants
    -- are added, ad hoc errors go here.
    BasicError String
  | -- | Bad primitive type
    PrimTypeBadError PrimTypeBadWhy
  | -- | Literal problem
    LiteralBadError LiteralBadWhy
  | -- | Type parsing error
    TypeParseError TypeParseError
  | -- | Unexpected end of file
    UnexpectedEOFError
  | -- | Internal error
    InternalError String
  | -- | Symbol error
    SymbolError SymbolError
  | -- | Expression parsing error
    ExprParseError ExprParseError
  deriving (Eq, Show)

-- | Why a primitive type couldn't be parsed, or is incorrect.
data PrimTypeBadWhy
  = -- | Too many specifiers of the same type (e.g. "multiple sign specifiers")
    TooManyPrimSpecifiers String
  | -- | Incompatible type combination (e.g. \"mixing void with float\")
    --
    --     If the list of \"other types\" is empty, then say,
    --     \"mixing [void] with other types\" (replace [void] with the type).
    IncompatibleTypes String [String]
  | -- | Unrecognized or invalid _BitInt width
    InvalidBitIntWidth Integer
  | -- | Invalid _BitInt width due to overflow or bad formatting
    InvalidBitIntWidthOverflowOrBadFormat
  | -- | Invalid _Decimal bits specification
    InvalidDecimalBits BitIntWidth
  | -- | Giving signedness to non-integral type
    InvalidSignedness String
  | -- | Constructing a decimal complex type
    InvalidDecimalComplex
  | -- | Empty or unsupported type specification
    InvalidTypeSpec String
  deriving (Eq)

-- | Cause for literal processing failure.
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

-- | Errors related to type parsing
data TypeParseError
  = -- | Token combination is invalid or unknown
    InvalidTokenCombination
      -- | Procedure name
      String
      -- | Best reconstructed type name
      String
  | -- | Too many type specifiers of a specific kind
    TooManyTypeSpecifiers String
  | -- | Unexpected abstract declarator in a context requiring an identifier
    MissingRequiredIdentifier
  | -- | Atomic type contains qualified types
    InvalidAtomicQualifiedType Type
  | -- | Unresolvable type name
    UnknownTypedefName Str
  | -- | Incompatible type categories
    IncompatibleTypeCategories String
  | -- | Bad pointer syntax
    BadPointerSyntax
  | -- | Bad struct or union definition
    BadStructOrUnionDefinition
  deriving (Eq, Show)

-- | Errors related to expression parsing
data ExprParseError
  = -- | Expected a specific token but didn't find it
    ExpectedToken String
  | -- | Missing comma or other separator
    MissingSeparator String
  | -- | Invalid expression in a specific context
    InvalidExpression String
  | -- | Expected an expression
    ExpectedExpression
  | -- | Expected a literal
    ExpectedLiteral
  | -- | Expected an identifier
    ExpectedIdentifier
  | -- | Error while parsing a generic expression
    MalformedGenericExpression
  deriving (Eq, Show)

-- | Symbol-related errors beyond redefinition
data SymbolError
  = -- | Symbol not found during lookup
    SymbolNotFound Str
  | -- | Used as the wrong kind (e.g., used a variable as a type)
    WrongSymbolKind SymbolKind SymbolKind
  | -- | Symbol scope violation
    ScopeViolation String
  | -- | Symbol already defined in current scope
    AlreadyDefinedInScope
  | -- | Type mismatch in redefinition
    TypeMismatch
  deriving (Eq, Show)

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
primtype2type pt = Type mempty mempty (BTPrim pt) mempty mempty AlignNone

instance Show PrimTypeBadWhy where
  show = \case
    TooManyPrimSpecifiers s -> printf "too many %s specifiers" s
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
  __throwable_insert st.symtab_names sym name
  (s :| _) <- symlatestscope "symgivename"
  __throwable_insert s name sym

-- | Do a cascading lookup for a symbol given the name from the current
-- symbol scope. If the symbol cannot be found, fail the parser instead of
-- throwing an error. Note: if the scope doesn't exist at all (empty stack),
-- then it is considered an error, so an error will be thrown.
symlookup :: Str -> Parser Symbol
symlookup name = do
  (s :| ss) <- symlatestscope "symlookup"
  (s : ss) & fix \r -> \case
    [] -> failed
    (t : tt) -> do
      liftIO (H.lookup t name) >>= \case
        Nothing -> r tt
        Just sym -> pure sym

-- | Look up the type for a symbol. If it isn't registered, fail.
symgettype :: Symbol -> Parser Type
symgettype sym = do
  tys <- symtab_types <$> asktab
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
  if oopsies && not (comp ^. comp_allow_block_shadowing)
    then err $ SymbolError AlreadyDefinedInScope
    else pure ()

-- | Associate a symbol with a type.
symassoctype :: Symbol -> Type -> Parser ()
symassoctype sym ty = do
  st <- asktab
  __throwable_insert st.symtab_types sym ty

-- | Get the latest scope as a list or error out.
symlatestscope :: String -> Parser (NonEmpty Str2Symbol)
symlatestscope procname = do
  s <- symtab_scopes <$> asktab
  readIORef s >>= \case
    ScopeStack [] -> err $ InternalError $ procname ++ ": empty stack"
    ScopeStack (t : ts) -> pure (t :| ts)

-- | Enter a new scope
enterscope :: Str2Symbol -> Parser ()
enterscope nametab = do
  st <- asktab
  modifyIORef' (symtab_scopes st) \(ScopeStack scopes) ->
    ScopeStack (nametab : scopes)

-- | Exit the current scope
exitscope :: Parser ()
exitscope = do
  st <- asktab
  (_ :| s) <- symlatestscope "exitscope"
  writeIORef st.symtab_scopes (ScopeStack s)

-- | Parse @p@ in the given symbol table and then after @p@ finishes pop it.
inscope :: Str2Symbol -> Parser a -> Parser a
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
-- For @char@, it depends on the signedness.
ist_preciseposbw :: PrimType -> Parser BitIntWidth
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
ist_precisebw :: PrimType -> Parser BitIntWidth
ist_precisebw (PTInt _ a) = case a of
  ILShort -> _ist_shortbitwidth <$> ain
  ILInt -> _ist_intbitwidth <$> ain
  ILLong -> _ist_longbitwidth <$> ain
  ILLongLong -> _ist_longlongbitwidth <$> ain
  ILBitInt n -> pure n
ist_precisebw (PTChar _) = _ist_charbitwidth <$> ain
ist_precisebw t = ist_preciseposbw t

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

-- | Default compliance settings.
--
--  - Allow block shadowing: ON
--  - Separate typedef and variable namespaces: ON
defaultcompliancesettings :: ComplianceSettings
defaultcompliancesettings =
  mempty
    & (comp_allow_block_shadowing .~ True)
      . (comp_separate_typedef_variable_ns .~ True)

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
mkrecord a b c d = Record a b c d <$> liftIO H.new

-- | Lifted 'DT.traceIO'
traceIO :: (MonadIO m) => String -> m ()
traceIO = liftIO . DT.traceIO

-- | Dump the symbol table
dbg_dumpsyms :: Parser ()
dbg_dumpsyms = do
  tab <- asktab
  -- print the scopes...
  let go i (sc : scs) = do
        traceIO $ printf "Scope %v:" i
        jj <- newIORef (0 :: Int)
        flip H.mapM_ sc \(name, sym) -> do
          typ <- H.lookup tab.symtab_types sym
          let typst = maybe "" show typ
          j <- readIORef jj <* modifyIORef' jj succ
          traceIO $ printf "  %v: %s %s" j (C8.unpack name) typst
        traceIO ""
        go (i + 1) scs
      go _ [] = pure ()
  ScopeStack scopes <- readIORef tab.symtab_scopes
  liftIO $ go (0 :: Int) scopes

-- | A 'Getter' to quickly decide if an initializer is empty (i.e., @{}@).
init_isempty :: Getter Initializer Bool
init_isempty = to \case
  InitBraced [] -> True
  _ -> False

instance Show DeclInit where
  show (DeclInit _ mi) = show mi

makeLenses ''Type

makeLenses ''Record

makeLenses ''ArrayType

makeLenses ''FuncInfo

makeLenses ''EnumType

makeLenses ''InitItem

makeLenses ''ParserState
