module Language.NC.CTypes
  ( Tag,
    Name (..),
    Expr,
    Field (..),
    Bind (..),
    Type (..),
    Variadic (..),
    UnionStruct (..),
    Record (..),
    EnumCase (..),
    Enumeration (..),
    Case (..),
    PrimType (..),
    pattern Int_,
    pattern Char_,
    pattern Float_,
    pattern Double_,
    pattern LongDouble_,
    pattern ComplexFloat_,
    pattern ComplexDouble_,
    pattern ComplexLongDouble_,
    pattern Short_,
    pattern Long_,
    pattern LongLong_,
    pattern UInt_,
    pattern UShort_,
    pattern ULong_,
    pattern ULongLong_,
    Signed (..),
    IntLen (..),
    FloatType (..),
    RealFloatType (..),
    Qualifier (..),
    QualifiedType (..),
    StorageClass (..),
    qualzero,
  )
where

import Language.NC.Internal.Prelude hiding
  ( Bool,
    Char,
    Double,
    Enum,
    Float,
    Int,
    const,
    span,
  )
import Language.NC.Internal.Prelude qualified as Pr

-- | For structs, unions, enums, and union structs.
type Tag = Name

-- | Name with optional source location and possibly other annotations.
data Name = Name
  { -- | Identifier in a 'Data.ByteString.Short.ShortByteString'.
    name :: Str,
    -- | Source location
    span :: Maybe Span
  }
  deriving (Eq, Show)

-- | Placeholder for expression; used for dependent typing in variable
-- sized arrays and for enumerations. Right now, you can't make an instance of
-- type, so any constructor that requires this type also can't be constructed.
data Expr
  deriving (Eq, Show)

-- | Fields for structs, unions, and union struct cases.
data Field
  = Field Type Name
  | -- | Bit fields are highly restricted. We allow them for all
    -- \"record\" types so we can report good errors.
    BitField Type Name
  deriving (Eq, Show)

-- | A binding in a @union struct@ case or a parameter list.
data Bind
  = -- | Extended syntax allows name-only patterns in certain situations
    -- where they would be disallowed in C.
    Bind (Maybe Type) (Maybe Name)
  deriving (Eq, Show)

-- | Type of a term. Anonymous record types, pointer decorations make them
-- complicated.
data Type
  = -- | Primitive, non-derived type
    PrimitiveT PrimType
  | StructT Record
  | UnionT Record
  | -- | The \'union struct\' is a new concept in this modification of C.
    -- It implements a true algebraic data type with a runtime tag
    -- to distinguish different cases.
    UnionStructT UnionStruct
  | EnumT Enumeration
  | -- | Potentially variadic function, with possibly missing argument list.
    -- (This situation is different from having a @void@ argument list,
    -- where it means it will take no arguments. With a missing argument list,
    -- the number of arguments to take is unknown or hidden.)
    FunctionT Variadic (Maybe (Seq Type)) Type
  | PointerT QualifiedType
  deriving (Eq, Show)

-- | Is the function variadic?
data Variadic
  = Variadic
  | NotVariadic
  deriving (Eq, Show)

-- | A @union struct@, that is, a tagged union.
data UnionStruct
  = -- | Name, cases, maybe a @default@ case.
    USDef (Maybe Name) (Seq Case) (Maybe (Seq Field))
  | -- | A @union struct@ declaration must give a name.
    USDecl Name
  deriving (Eq, Show)

-- | A @struct@ or @union@ type's body.
data Record
  = -- | Name, Cases.
    RecDef (Maybe Name) (Seq Name)
  | -- | A @struct@ or @union@ declaration must give a name.
    RecDecl Name
  deriving (Eq, Show)

-- | An @enum@ case.
data EnumCase
  = -- | Enum case given by name only.
    NameOnlyEC Name
  | -- | Constant expression evaluated enum case.
    ConstEC Name Expr
  deriving (Eq, Show)

-- | An @enum@.
data Enumeration
  = EnumDef (Maybe Tag) (Seq Field)
  | EnumDecl Tag
  deriving (Eq, Show)

-- | A case in a union struct. All cases must have a name, except for
-- a possible default clause. Case names live in the same namespace as tags do.
data Case = Case Name (Seq Bind)
  deriving (Eq, Show)

-- | Non-derived, primitive types
data PrimType
  = -- | @_Bool@
    Bool
  | -- | Non-character integral types, but not @char@ or @_Bool@
    Int Signed IntLen
  | -- | Character types
    Char (Maybe Signed)
  | -- | Real or complex floating point
    Float FloatType
  | -- | The @void@ type
    Void
  deriving (Eq, Show)

-- | Some convenience patterns for 'PrimType'
pattern Int_, Char_ :: PrimType
pattern Int_ = Int Signed IntLen
pattern Char_ = Char Nothing

-- | Some convenience patterns for 'PrimType'
pattern Float_, Double_, LongDouble_ :: PrimType
pattern Float_ = Float (Real RFFloat)
pattern Double_ = Float (Real RFDouble)
pattern LongDouble_ = Float (Real RFLongDouble)

-- | Some convenience patterns for 'PrimType'
pattern ComplexFloat_, ComplexDouble_, ComplexLongDouble_ :: PrimType
pattern ComplexFloat_ = Float (Complex RFFloat)
pattern ComplexDouble_ = Float (Complex RFDouble)
pattern ComplexLongDouble_ = Float (Complex RFLongDouble)

-- | Some convenience patterns for 'PrimType'
pattern Short_, Long_, LongLong_ :: PrimType
pattern Short_ = Int Signed Short
pattern Long_ = Int Signed Long
pattern LongLong_ = Int Signed LongLong

-- | Some convenience patterns for 'PrimType'
pattern UInt_, UShort_, ULong_, ULongLong_ :: PrimType
pattern UInt_ = Int Unsigned IntLen
pattern UShort_ = Int Unsigned Short
pattern ULong_ = Int Unsigned Long
pattern ULongLong_ = Int Unsigned LongLong

-- | Signed?
data Signed = Signed | Unsigned
  deriving (Eq, Show)

-- | @int@, @short@, @long@, or @long long@
data IntLen = IntLen | Short | Long | LongLong
  deriving (Eq, Show)

-- | Real or complex and what type?
data FloatType = Real RealFloatType | Complex RealFloatType
  deriving (Eq, Show)

-- | @float@, @double@, or @long double@?
data RealFloatType = RFFloat | RFDouble | RFLongDouble
  deriving (Eq, Show)

-- | Pointers need qualifiers
data QualifiedType = Qualified Qualifier Type
  deriving (Eq, Show)

-- | Type qualifiers (@const@, @volatile@, and, for referrents, @restrict@)
--
-- See also: 'qualzero'
data Qualifier = Qualifier
  { const :: Pr.Bool,
    volatile :: Pr.Bool,
    restrict :: Pr.Bool,
    -- | New idea, highly experimental. Similarly to @restrict@ (C99),
    -- @_Linear@ can only be attached to pointers in a parameter list.
    -- But with a crucial difference: whereas @restrict@ annotates the
    -- referent of the pointer, @_Linear@ annotates the pointer itself.
    -- As of writing this, a possible semantics of @_Linear@ is to
    -- make sure exactly one lvalue\/rvalue conversion occurrs.
    linear :: Pr.Bool
  }
  deriving (Eq, Show)

-- | Storage class and linkage specifiers. @_Thread_local@ is from C11.
data StorageClass = Static | Extern | Register | Auto | ThreadLocal | NoStorage
  deriving (Eq, Show)

-- | The null qualifier
qualzero :: Qualifier
qualzero = Qualifier False False False False
