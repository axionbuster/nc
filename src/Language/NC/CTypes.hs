module Language.NC.CTypes
  ( Tag,
    Name,
    Expr (..),
    Field (..),
    Type (..),
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
    PrimX,
    ArrX,
    StrX,
    UniX,
    UnsX,
    FunX,
    PtrX,
    CaseX,
    FldX,
    qualzero,
  )
where

import Language.NC.Internal.Prelude hiding (Bool, Char, Double, Enum, Float, Int, const)
import Language.NC.Internal.Prelude qualified as Pr

-- for extensibility, i'm using the Trees That Grow approach
-- for phase-dependent inline metadata.

-- | Occurrence name (not mangled; tag for a struct, union, etc.)
type Tag a = ByteString

-- | Occurrence name (not mangled)
type Name a = ByteString

-- | Placeholder for expression
data Expr a = Expr a

instance Eq (Expr a) where
  _ == _ = True

instance Show (Expr a) where
  show _ = "Expr"

-- | Fields for structs, unions, and union struct cases.
data Field a
  = Field (FldX a) (Type a) (Name a)
  | -- | Bit fields are highly restricted.
    BitField (FldX a) (Type a) (Name a)

deriving instance (PhaseEqShow a) => Show (Field a)

-- Struct/Union/UnionStruct symbols live in a different namespace than
-- terms, but Struct/Union/UnionStruct symbols share the same namespace.

-- | n1256. 6.7.2 "Type specifiers", with an addition of @Sum@
-- (`union struct`).
data Type a
  = Primitive (PrimX a) PrimType
  | -- | Array, known dimensions, dependent dimensions
    Array (ArrX a) (Type a) [Pr.Int] [Expr a] -- Q: what about renaming?
  | Struct (StrX a) (Name a) [Field a]
  | Union (UniX a) (Name a) [Field a]
  | -- | The \'union struct\' is a new concept in this modification of C.
    -- It implements a true algebraic data type with a runtime tag
    -- to distinguish different cases.
    UnionStruct (UnsX a) (Name a) [Field a]
  | -- | Potentially variadic function.
    --
    -- In C declaring a function prototype with an unknown number
    -- of arguments is allowed, and this is especially relevant
    -- for declaration of a function pointer. See this example:
    --
    -- @int f(int (\*)(), double (\*)[3]);@
    --
    -- Here the first argument may take any number of arguments.
    -- However, the return type must always be specified.
    Function (FunX a) (Maybe [Type a]) [Type a] (Type a)
  | Pointer (PtrX a) (QualifiedType a)

deriving instance (PhaseEqShow a) => Show (Type a)

-- | A case in a union struct. All cases must have a name.
-- Case names live in the same namespace as tags do.
data Case a = Case (CaseX a) (Name a) [Field a]

deriving instance (PhaseEqShow a) => Show (Case a)

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

pattern Float_, Double_, LongDouble_ :: PrimType
pattern Float_ = Float (Real RFFloat)
pattern Double_ = Float (Real RFDouble)
pattern LongDouble_ = Float (Real RFLongDouble)

pattern ComplexFloat_, ComplexDouble_, ComplexLongDouble_ :: PrimType
pattern ComplexFloat_ = Float (Complex RFFloat)
pattern ComplexDouble_ = Float (Complex RFDouble)
pattern ComplexLongDouble_ = Float (Complex RFLongDouble)

pattern Short_, Long_, LongLong_ :: PrimType
pattern Short_ = Int Signed Short
pattern Long_ = Int Signed Long
pattern LongLong_ = Int Signed LongLong

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
data QualifiedType a = Qualified (QualX a) Qualifier (Type a)

deriving instance (PhaseEqShow x) => Show (QualifiedType x)

-- | Type qualifiers (@const@, @volatile@, and, for referrents, @restrict@)
--
-- See also: 'qualzero'
data Qualifier = Qualifier
  {const :: Pr.Bool, volatile :: Pr.Bool, restrict :: Pr.Bool}
  deriving (Eq, Show)

-- | Storage class and linkage specifiers
data StorageClass = Static | Extern | Register | Auto | ThreadLocal | NoStorage
  deriving (Eq, Show)

-- | Constraints for all phase annotations, requiring Eq and Show
class
  ( Eq (PrimX a),
    Show (PrimX a),
    Eq (ArrX a),
    Show (ArrX a),
    Eq (StrX a),
    Show (StrX a),
    Eq (UniX a),
    Show (UniX a),
    Eq (UnsX a),
    Show (UnsX a),
    Eq (FunX a),
    Show (FunX a),
    Eq (PtrX a),
    Show (PtrX a),
    Eq (CaseX a),
    Show (CaseX a),
    Eq (FldX a),
    Show (FldX a),
    Eq (QualX a),
    Show (QualX a)
  ) =>
  PhaseEqShow a

type family PrimX a

type family ArrX a

type family StrX a

type family UniX a

type family UnsX a

type family FunX a

type family PtrX a

type family CaseX a

type family FldX a

type instance PrimX () = ()

type instance ArrX () = ()

type instance StrX () = ()

type instance UniX () = ()

type instance UnsX () = ()

type instance FunX () = ()

type instance PtrX () = ()

type instance CaseX () = ()

type instance FldX () = ()

-- | The null qualifier
qualzero :: Qualifier
qualzero = Qualifier False False False

type family QualX a

type instance QualX () = ()
