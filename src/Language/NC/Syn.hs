module Language.NC.Syn where

import Language.NC.Prelude hiding (Char, Double, Enum, Float, Int, const)
import Language.NC.Prelude qualified as Pr

-- for extensibility, i'm using the Trees That Grow approach
-- for phase-dependent inline metadata.

type Tag a = Text

type Name a = Text

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
    Array (ArrX a) (Type a) [Pr.Int] [Expr a]
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
    -- @int f(int (*)(), double (*)[3]);@
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

-- | A declaration. I'm not including attributes, yet.
data Dec a = Dec (DecX a) Linkage StorageClass (QualifiedType a) (Name a)

deriving instance (PhaseEqShow a) => Show (Dec a)

-- | Non-derived, primitive types
data PrimType
  = -- | non-character integral types
    Int Signed IntLen
  | -- | character types
    Char (Maybe Signed)
  | -- | real or complex floating point
    Float FloatType
  | -- | the @void@ type
    Void
  deriving (Eq, Show)

data Completeness = Complete | Incomplete
  deriving (Eq, Show)

data Signed = Signed | Unsigned
  deriving (Eq, Show)

data IntLen = IntLen | Short | Long | LongLong
  deriving (Eq, Show)

data FloatType = Real RealFloatType | Complex RealFloatType
  deriving (Eq, Show)

data RealFloatType = RFFloat | RFDouble | RFLongDouble
  deriving (Eq, Show)

data QualifiedType a = Qualified (QualX a) Qualifier (Type a)

deriving instance (PhaseEqShow x) => Show (QualifiedType x)

data Qualifier = Qualifier
  {const :: Bool, volatile :: Bool, restrict :: Bool}
  deriving (Eq, Show)

-- | Storage class and linkage specifiers
data StorageClass = Static | Extern | Register | Auto | ThreadLocal | NoStorage
  deriving (Eq, Show)

-- | Separate linkage type if needed
data Linkage = Internal | External | NoLinkage
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
    Eq (DecX a),
    Show (DecX a),
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

type family DecX a

type family CaseX a

type family FldX a

type instance PrimX () = ()

type instance ArrX () = ()

type instance StrX () = ()

type instance UniX () = ()

type instance UnsX () = ()

type instance FunX () = ()

type instance PtrX () = ()

type instance DecX () = ()

type instance CaseX () = ()

type instance FldX () = ()

qualzero :: Qualifier
qualzero = Qualifier False False False

type family QualX a

type instance QualX () = ()
