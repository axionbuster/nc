module Language.NC.CTypes
  ( module Language.NC.Experiment.PrimTypes,
    Tag,
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
    Qualifier (..),
    QualifiedType (..),
    StorageClass (..),
    qualzero,
  )
where

import Language.NC.Expr
import Language.NC.Internal.Prelude hiding
  ( const,
    span,
  )
import Language.NC.Experiment.PrimTypes
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

-- | Fields for structs, unions, and union struct cases.
data Field
  = Field Type Name
  | BitField Type Name
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
  | -- | C struct type.
    StructT Record
  | -- | C (untagged) union.
    UnionT Record
  | -- | The \'union struct\' is a concept in New C.
    -- It represents a true algebraic data type with a runtime tag
    -- to distinguish different cases.
    UnionStructT UnionStruct
  | -- | C union type.
    EnumT Enumeration
  | -- | Potentially variadic C function, with possibly missing argument list.
    -- (This situation is different from having a @void@ argument list,
    -- where it means it will take no arguments. With a missing argument list,
    -- the number of arguments to take is unknown or hidden.)
    CFunctionT Variadic (Maybe (Seq Type)) Type
  | -- | Regular function. No variadic functions. All arguments must be
    -- specified at all times, even in a parameter list.
    FunctionT (Seq Type) Type
  | -- | Pointer type
    PointerT QualifiedType
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
