module Language.NC.Syn where

import Language.NC.Prelude hiding (Char, Double, Enum, Float, Int, const)
import Language.NC.Prelude qualified as Pr

-- for extensibility, i'm using the Trees That Grow approach
-- for phase-dependent inline metadata.

type Tag a = Text

type Name a = Text

-- | Placeholder for expression
data Expr a = Expr a

-- | Fields for structs, unions, and union struct cases.
data Field a
  = Field (FldX a) (Type a) (Name a)
  | -- | Bit fields are highly restricted.
    BitField (FldX a) (Type a) (Name a)

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

-- | A case in a union struct. All cases must have a name.
-- Case names live in the same namespace as tags do.
data Case a = Case (CaseX a) (Name a) [Field a]

-- | A declaration. I'm not including attributes, yet.
data Dec a = Dec (DecX a) Linkage StorageClass (QualifiedType a) (Name a)

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

data Completeness = Complete | Incomplete

data Signed = Signed | Unsigned

data IntLen = IntLen | Short | Long | LongLong

data FloatType = Real RealFloatType | Complex RealFloatType

data RealFloatType = RFFloat | RFDouble | RFLongDouble

data QualifiedType a = Qualified (QualX a) Qualifier (Type a)

data Qualifier = Qualifier
  {const :: Bool, volatile :: Bool, restrict :: Bool}

qualzero :: Qualifier
qualzero = Qualifier False False False

type family QualX a

type instance QualX () = ()

-- | Storage class and linkage specifiers
data StorageClass = Static | Extern | Register | Auto | ThreadLocal | NoStorage

-- | Separate linkage type if needed
data Linkage = Internal | External | NoLinkage
