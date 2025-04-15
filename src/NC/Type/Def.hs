{-# LANGUAGE StrictData #-}

module NC.Type.Def where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (LazyByteString)
import Data.Coerce
import Data.HashTable.IO qualified as H
import Data.Hashable
import Data.Unique
import NC.Type.Prim
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

-- | Unqualified type
data UQType
  = UQPrim Prim
  | UQFunc FuncInfo
  | UQPointer Type
  | UQArray ArrayInfo
  | UQRecord RecInfo
  | UQEnum EnumInfo
  | UQAtomic UQType
  | UQTypeof TypeofQual TypeofInfo

-- | A C attribute
data Attribute = Attribute
  { _attr_prefix :: Maybe Name,
    _attr_basename :: Name,
    _attr_info :: Maybe Span64
  }

-- | A C qualification
newtype Qual = Qual Word16
  deriving (Eq, Show, Bits)
  deriving (Semigroup, Monoid) via (Ior Qual)

-- | A C alignment specification
data Alignment
  = AlignNone
  | AlignAs ConstIntExpr
  | AlignAsType Type

-- *** C Record types (struct, union)

-- | Information about a C record type
data RecInfo = RecInfo
  { _rec_type :: RecType,
    _rec_sym :: Symbol,
    _rec_attrs :: [Attribute],
    _rec_def :: Maybe [RecMember]
  }

-- | Is the record a @struct@ or a @union@?
data RecType = RecStruct | RecUnion

-- | A C record member may be a field, which can be a bitfield, or
-- it may be a @static\_assert@.
data RecMember
  = RMField [Attribute] Type Symbol (Maybe ConstIntExpr)
  | RMStaticAssertion StaticAssertion

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

-- | The outermost derivation of an array in parameter position may be
-- given extra annotations.
data ParamArrayInfo = ParamArrayInfo
  { -- | If set, there will be at least a given number of elements. This
    -- implies that the array will exist.
    _parr_static :: ArrayStatic,
    -- | Qualifications for the decayed pointer.
    _parr_qual :: Qual
  }

-- | In parameter position, and in the outermost derivation, an array can be
-- given a @static@ annotation, then the array exists (is not null), and
-- there will be _at least_ a given number of elements.
data ArrayStatic
  = ASNoStatic
  | ASStatic

-- *** C Enumeration types

-- | A C @enum@ type
data EnumInfo = EnumInfo
  { _enum_sym :: Symbol,
    _enum_attrs :: [Attribute],
    _enum_membertype :: Type,
    _enum_members :: Maybe [EnumConst]
  }

-- | A constant declared in a C @enum@
data EnumConst = EnumConst
  { _ec_sym :: Symbol,
    _ec_attrs :: [Attribute],
    _ec_explicitval :: Maybe ConstIntExpr
  }

-- *** C Functions

-- | C function type
data FuncInfo = FuncInfo
  { _fun_rettype :: Type,
    _fun_pars :: [Param],
    _fun_variadic :: Variadic
  }

-- | A function parameter that optionally introduces an identifier into
-- scope.
data Param = Param Type (Maybe Symbol)

-- | Is a function variadic?
data Variadic = Variadic | NotVariadic

-- *** C Metaprogramming types

-- | Argument of @typeof@ or @typeof\_unqual@
data Typeof = TypeofType Type | TypeofExpr Expr

-- | Is it a @typeof@ or @typeof\_unqual@?
data TypeofQual = TQQual | TQUnqual

-- ** C Declarations, expressions, statements, and initializers.

-- *** C declarations

-- | A C declarator.
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

-- | A pair of declarator and optional initializer.
data DeclInit
  = -- | Fully determined type for each declarator, and optional initializer.
    -- DeclInit Type StorageClass (Maybe Initializer)
    DeclInit
    { -- | Fully determined type.
      _decl_type :: Type,
      -- | Storage class.
      _decl_stor :: StorageClass,
      -- | Initializer (optional).
      _decl_init :: Maybe Initializer
    }

-- | A @static\_assertion@.
data StaticAssertion
  = -- | An expression that must not evaluate to 0, and optional message
    -- that should be emitted if it does.
    StaticAssertion ConstIntExpr (Maybe StringLiteral)

-- | A C storage class specifier
data StorageClass
  = SExtern
  | SAuto
  | SStatic
  | STypedef
  | SRegister
  | SThreadLocal
  | SConstExpr

-- *** C Expressions

-- | A C expression. We pay attention to have only up to four constructors.
data Expr
  = Expr PrimExpr
  | ExprUnary UnaryOp Expr
  | ExprBinary BinOp Expr Expr
  | ExprSpecial SpecialExpr

pattern ExprPostInc, ExprPostDec, ExprPreInc, ExprPreDec :: Expr -> Expr
pattern ExprPostInc e = ExprUnary UOPostInc e
pattern ExprPostDec e = ExprUnary UOPostDec e
pattern ExprPreInc e = ExprUnary UOPreInc e
pattern ExprPreDec e = ExprUnary UOPreDec e

pattern ExprUnaryPlus, ExprUnaryMinus, ExprLogNot :: Type -> Type
pattern ExprUnaryPlus e = ExprUnary UOPlus e
pattern ExprUnaryMinus e = ExprUnary UOMinus e
pattern ExprLogNot e = ExprUnary UONot e

pattern ExprBitNot, ExprDeref, ExprAddrOf :: Type -> Type
pattern ExprBitNot e = ExprUnary UOBitNot e
pattern ExprDeref e = ExprUnary UODeref e
pattern ExprAddrOf e = ExprUnary UOAddrOf e

pattern ExprArray, ExprTimes, ExprDiv :: Expr -> Expr -> Expr
pattern ExprArray e f = ExprBinary BOArray e f
pattern ExprTimes e f = ExprBinary BOTimes e f
pattern ExprDiv e f = ExprBinary BODiv e f

pattern ExprMod, ExprPlus, ExprMinus :: Expr -> Expr -> Expr
pattern ExprMod e f = ExprBinary BOMod e f
pattern ExprPlus e f = ExprBinary BOPlus e f
pattern ExprMinus e f = ExprBinary BOMinus e f

pattern ExprShiftL, ExprShiftR, ExprLT :: Expr -> Expr -> Expr
pattern ExprShiftL e f = ExprBinary BOShiftL e f
pattern ExprShiftR e f = ExprBinary BOShiftR e f
pattern ExprLT e f = ExprBinary BOLT e f

pattern ExprGT, ExprLE, ExprGE :: Expr -> Expr -> Expr
pattern ExprGT e f = ExprBinary BOGT e f
pattern ExprLE e f = ExprBinary BOLE e f
pattern ExprGE e f = ExprBinary BOGE e f

pattern ExprEQ, ExprNE, ExprBitAnd :: Expr -> Expr -> Expr
pattern ExprEQ e f = ExprBinary BOEQ e f
pattern ExprNE e f = ExprBinary BONE e f
pattern ExprBitAnd e f = ExprBinary BOBitAnd e f

pattern ExprBitXor, ExprBitOr, ExprLogAnd :: Expr -> Expr -> Expr
pattern ExprBitXor e f = ExprBinary BOBitXor e f
pattern ExprBitOr e f = ExprBinary BOBitOr e f
pattern ExprLogAnd e f = ExprBinary BOLogAnd e f

pattern ExprLogOr, ExprAssign, ExprAssignPlus :: Expr -> Expr -> Expr
pattern ExprLogOr e f = ExprBinary BOLogOr e f
pattern ExprAssign e f = ExprBinary BOAssignExprAssign e f
pattern ExprAssignPlus e f = ExprBinary BOAssignPlus e f

pattern ExprAssignMinus, ExprAssignTimes, ExprAssignDiv :: Expr -> Expr -> Expr
pattern ExprAssignMinus e f = ExprBinary BOAssignMinusExprAssignMinus e f
pattern ExprAssignTimes e f = ExprBinary BOAssignTimesExprAssignTimes e f
pattern ExprAssignDiv e f = ExprBinary BOAssignDiv e f

pattern ExprAssignShiftL, ExprAssignShiftR :: Expr -> Expr -> Expr
pattern ExprAssignShiftL e f = ExprBinary BOAssignShiftLExprAssignShiftL e f
pattern ExprAssignShiftR e f = ExprBinary BOAssignShiftRExprAssignShiftR e f

pattern ExprAssignBitAnd :: Expr -> Expr -> Expr
pattern ExprAssignBitAnd e f = ExprBinary BOAssignBitAnd e f

pattern ExprAssignBitOr, ExprSequence :: Expr -> Expr -> Expr
pattern ExprAssignBitOr e f = ExprBinary BOAssignBitOrExprAssignBitOr e f
pattern ExprSequence e f = ExprBinary BOSequence e f

pattern ExprCall :: Expr -> [Expr] -> Expr
pattern ExprCall e fs = ExprSpecial (SECall e fs)

pattern ExprMember :: Expr -> Symbol -> Expr
pattern ExprMember e s = ExprSpecial (SEMember e s)

pattern ExprCompoundLiteral :: Type -> Initializer -> Expr
pattern ExprCompoundLiteral t i = ExprSpecial (SECompoundLiteral t i)

pattern ExprSizeof :: (Either Expr Type) -> Expr
pattern ExprSizeof et = ExprSpecial (SESizeof et)

pattern ExprAlignof :: (Either ConstIntExpr Type) -> Expr
pattern ExprAlignof et = ExprSpecial (SEAlignof et)

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

-- | Generic binary opration ('Expr' x 'Expr')
data BinOp
  = BOArray
  | BOTimes
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
  | BOAssignDiv
  | BOAssignMod
  | BOAssignShiftL
  | BOAssignShiftR
  | BOAssignBitAnd
  | BOAssignBitXor
  | BOAssignBitOr
  | -- | Comma (`,`)
    BOSequence

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

-- | A primary expression.
data PrimExpr
  = PrimId Name
  | PrimLit Lit
  | PrimParen Expr
  | PrimGeneric Expr [GenAssoc]

-- | A generic association, part of a generic selection.
data GenAssoc
  = -- | if no type, then @default@ case.
    GenAssoc (Maybe Type) Expr

-- | A constant integer expression.
data ConstIntExpr
  = CIEResolved Integer
  | CIEUnresolved Expr

-- *** C Statements

-- | A C statement.
type Statement = StmtBody

-- | A C statement.
data StmtBody
  = StmtExpr [Attribute] (Maybe Expr)
  | StmtCompound (Seq BlockItem)
  | StmtIf Expr Statement (Maybe Statement)
  | StmtSwitch Expr Statement
  | StmtWhile Expr Statement
  | StmtDoWhile Statement Expr
  | StmtFor ForInit (Maybe Expr) (Maybe Expr) Statement
  | StmtJump Jump
  | StmtLabeled Label (Maybe Statement)
  | StmtAttribute [Attribute]

-- | A 'BlockItem' can either be a declaration, a statement, or a label.
-- A standalone label is translated as a labeled null statement.
data BlockItem
  = BIDecl Declaration
  | BIStmt Statement

-- | A 'label' can be given by an identifier, a @case \_:@, or @default:@.
data Label
  = LabelNamed [Attribute] Symbol
  | LabelCase [Attribute] Symbol ConstIntExpr
  | LabelDefault [Attribute] Symbol

-- | The first clause of a @for@ declaration can either be a declaration
-- or an expression. The expression can be null.
data ForInit = FIDecl Declaration | FIExpr (Maybe Expr)

-- | A jump can be either a @return \_@, @break@, @continue@,
-- or a @goto \_@.
data Jump
  = JumpReturn (Maybe Expr)
  | JumpBreak
  | JumpContinue
  | JumpGoto Name

-- *** C Initializers

-- | An initializer.
data Initializer = InitExpr Expr | InitBraced [InitItem]

-- | Member of a braced initializer.
data InitItem = InitItem
  { -- | Example: @.vector[2][3].x@ (followed by @=@ and then value).
    _init_designation :: [Designator],
    -- | Value. It recurses.
    _init_value :: Initializer
  }

-- | A C designator may be an array index or a member access.
data Designator
  = -- | Syntax: @[constant-expression]@
    DesignatorIndex ConstIntExpr
  | -- | Syntax: @.identifier@
    DesignatorMember Name

-- ** C Literals

-- | A C Literal
data Lit
  = -- | Integer literal
    LitInteger IntegerLiteral
  | -- | Character constant
    LitChar CharacterLiteral
  | -- | String literal
    LitString StringLiteral
  deriving (Eq, Show)

-- | An integer literal, annotated with type
data IntegerLiteral
  = IntegerLiteral Integer Prim
  deriving (Eq, Show)

-- | A character literal, annotated with type
data CharacterLiteral
  = -- | Literal character or universal character name, encoding choice delayed.
    CharacterLiteral Char Prim
  | -- | Specified in octal or hexadecimal.
    IntCharacterLiteral Integer Prim
  deriving (Eq, Show)

-- | A string literal; escape sequences have been interpreted, but
-- a NUL byte has NOT been inserted at the end. The encoding is:
--
--  - UTF-8 for single-byte-character strings
--  - UTF-16 for two-byte-character strings
--  - UTF-32 for four-byte-character strings
data StringLiteral
  = -- | Interpreted value, type of each element.
    StringLiteral LazyByteString Prim
  deriving (Show, Eq)

-- | It will merely say @"\<symbol\>"@
instance Show Symbol where
  show _ = "<symbol>"

instance Hashable Symbol where
  hashWithSalt salt sym = hashWithSalt @Int salt (coerce hashUnique sym)
