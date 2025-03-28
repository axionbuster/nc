-- | C23-based types, later augmented with extensions.
module Language.NC.Experiment.Types
  ( module Language.NC.Experiment.PrimTypes,
    Name (..),
    Storage (..),
    Attribute (..),
    StandardAttribute (..),
    Qualifier (..),
    QualifierKey (..),
    QConst (..),
    QVolatile (..),
    QRestrict (..),
    QAtomic (..),
    Inline (..),
    EffectKey (..),
    Effects (..),
    ENoReturn (..),
    CEnum (..),
    CRecordField (..),
    CRecord (..),
    CSpecAtomic (..),
    CFunction (..),
    CArray (..),
    UnionStructCase (..),
    CUnionStruct (..),
    Pretype (..),
    Type (..),
    onpretype,
    DataCategory (..),
    catdata,
    Completeness (..),
    iscomplete,
  )
where

import Language.NC.Experiment.PrimTypes
import Language.NC.Internal.Prelude

data Name
  = -- | Name with optional unique identifier and optional occurrence name.
    Name !Unique (Maybe ShortByteString)
  deriving (Eq)

instance Show Name where
  show (Name _ (Just n)) = show n
  show (Name _ Nothing) = "<unnamed>"

instance Hashable Name where
  hashWithSalt s (Name u _) = hashWithSalt s u

-- | Storage-class specifiers, typically simply called
-- \"specifiers\" or \"storage classes\". At most one
-- of these can be used in a declaration.
--
-- Note: \"typedef\" is NOT an actual storage class, but merely
-- occupies that position in the C grammar.
data Storage
  = -- | Automatic storage duration
    Auto
  | -- | Register storage duration
    Register
  | -- | Static storage duration
    Static
  | -- | External linkage
    Extern
  | -- | Thread-local storage duration
    ThreadLocal
  | -- | Constant expression
    Constexpr
  | -- | No storage class specified
    NoSpecifier
  deriving (Eq, Show)

data Attribute
  = StandardAttribute StandardAttribute
  | -- | namespace, name; value not supported yet
    CustomAttribute Name Name
  deriving (Eq, Show)

data StandardAttribute
  = SAFallthrough
  | SADeprecated
  | SAMaybeUnused
  | SANoDiscard
  | SANoReturn
  | SAUnsequenced
  | SAReproducible
  deriving (Eq, Show)

data Qualifier = Qualifier
  { qconst :: QConst,
    qvolatile :: QVolatile,
    qrestrict :: QRestrict,
    qatomic :: QAtomic
  }
  deriving (Eq, Show)

data QConst = QNotConst | QConst deriving (Eq, Show, Ord, Enum)

data QVolatile = QNotVolatile | QVolatile deriving (Eq, Show, Ord, Enum)

data QRestrict = QNotRestrict | QRestrict deriving (Eq, Show, Ord, Enum)

data QAtomic = QNotAtomic | QAtomic deriving (Eq, Show, Ord, Enum)

data QualifierKey
  = QKConst
  | QKVolatile
  | QKRestrict
  | QKAtomic
  deriving (Eq, Show)

-- | Only use the (+) and (*) operators. This allows using the
-- 'Data.Semigroup.Sum' and 'Data.Semigroup.Product' monoids.
--
-- - (+): does inclusive OR on each field.
-- - (*): same but does an AND instead.
instance Num Qualifier where
  Qualifier c0 v0 r0 a0 + Qualifier c1 v1 r1 a1 =
    Qualifier (max c0 c1) (max v0 v1) (max r0 r1) (max a0 a1)
  Qualifier c0 v0 r0 a0 * Qualifier c1 v1 r1 a1 =
    Qualifier (min c0 c1) (min v0 v1) (min r0 r1) (min a0 a1)
  fromInteger = error "Qualifier fromInteger does not make sense"
  signum = error "Qualifier signum does not make sense"
  abs = error "Qualifier abs does not make sense"
  negate = error "Qualifier negate does not make sense"
  (-) = error "Qualifier subtract does not make sense"

data CEnum
  = -- | Backing type (default to @int@) and list of enumerators
    CEnum PrimType (Seq (Name, Maybe Int))
  deriving (Eq, Show)

data CRecordField
  = -- | Regular field. Anonymous field are only supported
    -- for anonymous struct and union members.
    CField Name Pretype
  | -- | Anonymous field. Only supported for struct and union members.
    CAnonymousField Pretype
  | -- | Bitfield with optional name, backing integral type, and width.
    CBitfield (Maybe Name) PrimType Int
  deriving (Eq, Show)

newtype CRecord
  = -- | Record containing fields
    CRecord (Seq CRecordField)
  deriving (Eq, Show)

-- | Atomically specified types, having the same representation but
-- different semantics. This is NOT THE SAME as atomically-
-- qualified types, which have the same semantics but
-- different representation. In short,
--
-- * Atomic specifier: new type, same rep.
-- * Atomic qualifier: same type, new rep.
newtype CSpecAtomic = CSpecAtomic Pretype deriving (Eq, Show)

-- | Is this function marked \'inline\'?
data Inline = MarkedInline | NotMarkedInline deriving (Eq, Show, Ord, Enum)

-- | Effect attribute to a function.
data EffectKey
  = -- | xref: 'SANoreturn'
    EKNoReturn
  deriving (Eq, Show)

data ENoReturn = ENoReturn | ENotNoReturn deriving (Eq, Show, Ord, Enum)

data Effects
  = Effects {enoreturn :: ENoReturn}
  deriving (Eq, Show)

-- Since C23, all functions must have a prototype,
-- and varargs don't have to be preceded by a parameter.
data CFunction
  = -- | Regular function
    CRegularFunction (Seq Type) Type Effects
  | -- | Vararg function
    CVarargFunction (Seq Type) Type Effects
  deriving (Eq, Show)

-- Currently variably-sized arrays are not supported
-- until I find out how to encode dependent types.
data CArray
  = -- | Unknown size
    CArrayUnsized Type
  | -- | Known size
    CArraySized Type Int
  deriving (Eq, Show)

data UnionStructCase
  = -- | represented by a single field
    SimpleCase Name Pretype
  | -- | represented by a struct
    RecordCase Name CRecord
  deriving (Eq, Show)

-- | Non-C type. A @union struct@ is a tagged union.
data CUnionStruct
  = -- | A @union struct@, that is, a tagged union,
    -- with a backing type, cases (if any) and optional default case.
    -- Allow empty definitions.
    CUnionStruct PrimType (Seq UnionStructCase) (Maybe Pretype)
  deriving (Eq, Show)

-- | C pretypes, which are types that can be used to declare
-- variables, but are not qualified. Officially called "type
-- specifiers" in the C standard.
data Pretype
  = PPrim PrimType
  | PPointer Type
  | PFunction CFunction
  | PStruct (Maybe CRecord) Name
  | PUnionStruct (Maybe CUnionStruct) Name
  | PArray CArray
  | PUnion (Maybe CRecord) Name
  | PEnum CEnum Name
  | PAtomic CSpecAtomic
  deriving (Eq, Show)

-- | Fully qualified and specified type, possibly with attributes.
data Type = Type
  { typequal :: [Qualifier],
    typeattr :: [Attribute],
    typetype :: Pretype
  }
  deriving (Eq, Show)

-- | Apply a function to the pretype of a type.
onpretype :: (Pretype -> a) -> Type -> a
onpretype = (. typetype)

-- | A variable is either an object, array, function, or
-- the type @void@.
data DataCategory
  = CatObject
  | CatArray
  | CatFunction
  | CatVoid
  deriving (Eq, Show)

-- | Categorize a pretype.
catdata :: Pretype -> DataCategory
catdata (PPrim Void) = CatVoid
catdata (PPrim {}) = CatObject
catdata (PPointer {}) = CatObject
catdata (PFunction {}) = CatFunction
catdata (PStruct {}) = CatObject
catdata (PUnionStruct {}) = CatObject
catdata (PArray {}) = CatArray
catdata (PUnion {}) = CatObject
catdata (PEnum {}) = CatObject
catdata (PAtomic {}) = CatObject

-- | Is a type complete?
data Completeness = Complete | Incomplete deriving (Eq, Show)

-- | Check if a (pre)type is complete.
iscomplete :: Pretype -> Completeness
iscomplete (PPrim Void) = Incomplete
iscomplete (PPrim {}) = Complete
iscomplete (PPointer {}) = Complete
iscomplete (PFunction {}) = Complete
iscomplete (PStruct {}) = error "not implemented: iscomplete PStruct"
iscomplete (PUnionStruct {}) = error "not implemented: iscomplete PUnionStruct"
iscomplete (PArray (CArrayUnsized {})) = Incomplete
iscomplete (PArray (CArraySized {})) = Complete
iscomplete (PUnion {}) = error "not implemented: iscomplete PUnion"
iscomplete (PEnum {}) = Complete
iscomplete (PAtomic {}) = Complete
