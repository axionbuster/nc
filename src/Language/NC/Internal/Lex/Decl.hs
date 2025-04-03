{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | This module implements parsing of C23 declarations.
--
-- A declaration in C defines types, variables, functions, and type aliases.
-- The module provides a clean interface for working with C23 declarations
-- while handling the complex syntax internally.
--
-- The design follows these principles:
-- 1. Simplified, flat structure for external use
-- 2. Flexible parsing using container-based approaches
-- 3. Strong integration with the type system
-- 4. Convenient access using optics (lenses, prisms)
module Language.NC.Internal.Lex.Decl (
  -- * Core Declaration Types
  Declaration (..),
  Declarator (..),
  Initializer (..),

  -- * Declaration Constructors

  -- These are exported through Declaration(..)

  -- * Components
  ParamDecl (..),
  Attributes (..),
  Attribute (..),
  AttributeValue (..),
  DesignatedInit (..),
  Designator (..),

  -- * Optics

  -- | Lenses for accessing and modifying declaration components
  declnam,
  decltyp,
  declattr,
  declparam,
  declret,
  declinit,
  attrnam,
  attrval,
  attrvals,

  -- * Parsers
  declaration,
  declarators,
  initializer,
  attributes,
) where

import Control.Lens
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Language.NC.Internal.Lex.Lex
import Language.NC.Internal.Lex.Op
import Language.NC.Internal.Lex.Type
import Language.NC.Internal.Prelude

-- * Exported types

-- | The primary declaration type representing all C declarations.
-- This provides a unified interface while supporting specific declaration kinds.
data Declaration
  = -- | Variable or object declaration
    VarDeclaration
      { -- | Name of the declared variable
        _declnam :: Symbol,
        -- | Type of the variable
        _decltyp :: Type,
        -- | Attributes applied to the declaration
        _declattr :: Attributes,
        -- | Optional initializer
        _declinit :: Maybe Initializer
      }
  | -- | Type definition (typedef)
    TypeDeclaration
      { -- | Name of the type alias
        _declnam :: Symbol,
        -- | The aliased type
        _decltyp :: Type,
        -- | Attributes applied to the type
        _declattr :: Attributes
      }
  | -- | Function declaration or definition
    FuncDeclaration
      { -- | Function name
        _declnam :: Symbol,
        -- | Function parameters
        _declparam :: [ParamDecl],
        -- | Return type
        _declret :: Type,
        -- | Function attributes
        _declattr :: Attributes
      }
  | -- | Static assertion
    StaticAssertDeclaration
      { -- | The assertion expression
        _asrtexpr :: Expr,
        -- | Optional message
        _asrtmsg :: Maybe Str
      }
  | -- | Standalone attribute declaration
    AttributeOnlyDeclaration
      { -- | The attributes
        _declattr :: Attributes
      }
  deriving (Eq, Show)

-- | A declarator describes how a base type is modified to form a complete type
data Declarator
  = -- | Simple identifier declarator
    IdDeclarator Symbol Attributes
  | -- | Pointer declarator
    PtrDeclarator Declarator TypeQual Attributes
  | -- | Array declarator
    ArrayDeclarator Declarator ArraySize Attributes
  | -- | Function declarator
    FuncDeclarator Declarator [ParamDecl] Variadic Attributes
  deriving (Eq, Show)

-- | Initializer for variables and objects
data Initializer
  = -- | Simple expression initializer
    ExprInit Expr
  | -- | Braced initializer list
    ListInit [DesignatedInit]
  deriving (Eq, Show)

-- | A parameter declaration within a function
data ParamDecl = ParamDecl
  { -- | Parameter name (optional)
    _paramnam :: Maybe Symbol,
    -- | Parameter type
    _paramtyp :: Type,
    -- | Parameter attributes
    _paramattr :: Attributes
  }
  deriving (Eq, Show)

-- | A collection of attributes applied to a declaration
newtype Attributes = Attributes
  { -- | HashMap of attribute names to values
    _attrvals :: HashMap Symbol AttributeValue
  }
  deriving (Eq, Show)

-- | An individual attribute with name and value
data Attribute = Attribute
  { -- | Attribute name
    _attrnam :: Symbol,
    -- | Attribute value
    _attrval :: AttributeValue
  }
  deriving (Eq, Show)

-- | The value of an attribute
data AttributeValue
  = -- | Flag-only attribute (no value)
    AttrFlag
  | -- | String value
    AttrStr Str
  | -- | Expression value
    AttrExpr Expr
  deriving (Eq, Show)

-- | An initializer with optional designator
data DesignatedInit = DesignatedInit
  { _designator :: Maybe Designator,
    _initvalue :: Initializer
  }
  deriving (Eq, Show)

-- | Designator for specifying which member to initialize
data Designator
  = -- | Array index designator [index]
    ArrayDesignator Expr
  | -- | Member designator .member
    MemberDesignator Symbol
  deriving (Eq, Show)

-- * Internal types (concrete syntax tree fragments, only for parsing)

-- | Intermediate representation for flexible parsing
data DeclComponents = DeclComponents
  { -- | Declaration specifiers in any order
    _dcspecs :: HashSet DeclSpecifier,
    -- | Raw declarators before processing
    _dcdeclarators :: [RawDeclarator],
    -- | Attributes in raw form
    _dcattributes :: [Attribute]
  }

-- | Raw specifier collected during parsing
data DeclSpecifier
  = DSStorageClass StorageClass
  | DSType Type
  | DSTypeQual TypeQual
  | DSFuncSpec FuncSpec
  deriving (Eq, Show)

-- | Raw declarator before semantic processing
data RawDeclarator = RawDeclarator
  { _rdname :: Maybe Symbol,
    _rdderivers :: [TypeDeriver],
    _rdattrs :: [Attribute]
  }
  deriving (Eq, Show)

-- | Type derivation operations
data TypeDeriver
  = TDPointer TypeQual
  | TDArray (Maybe Expr)
  | TDFunction [ParamDecl] Variadic
  deriving (Eq, Show)

-- Generate lenses, prisms and traversals
makeLenses ''Declaration
makeLenses ''ParamDecl
makeLenses ''Attributes
makeLenses ''Attribute
makeLenses ''DesignatedInit
makeLenses ''DeclComponents
makeLenses ''RawDeclarator

-- Generate prisms for Declaration constructors
makePrisms ''Declaration
makePrisms ''Declarator
makePrisms ''Initializer
makePrisms ''AttributeValue
makePrisms ''Designator

{-
-- private lenses
asrtmsg,
asrtexpr,
paramtyp,
paramnam,
paramattr,
dcspecs,
dcdeclarators,
dcattributes,
rdname,
rdderivers,
rdattrs,
-}

-- | Convert set of specifiers to appropriate type and storage information
resolvespecifiers :: HashSet DeclSpecifier -> (Type, StorageClass, FuncSpec)
resolvespecifiers _specs = error "resolvespecifiers: Not implemented yet"

-- | Convert raw declarator to processed Declarator
processdeclarator :: RawDeclarator -> Type -> Declarator
processdeclarator _raw _baseType =
  error "processdeclarator: Not implemented yet"

-- | Create final declaration from components
makedeclaration :: DeclComponents -> Either [Error] [Declaration]
makedeclaration _comps = error "makedeclaration: Not implemented yet"

-- | Empty attributes container
emptyattrs :: Attributes
emptyattrs = Attributes HashMap.empty

-- | Add an attribute to an Attributes container
addattr :: Attribute -> Attributes -> Attributes
addattr (Attribute name val) (Attributes attrs) =
  Attributes $ HashMap.insert name val attrs

-- | Create an attribute from name and value
mkattr :: Symbol -> AttributeValue -> Attribute
mkattr = Attribute

-- | Create a flag attribute (no value)
flagattr :: Symbol -> Attribute
flagattr name = Attribute name AttrFlag

-- | Placeholder: Parse a complete declaration
declaration :: Parser (WithSpan Declaration)
declaration = error "declaration: Not implemented yet"

-- | Placeholder: Parse a list of declarators
declarators :: Parser [Declarator]
declarators = error "declarators: Not implemented yet"

-- | Placeholder: Parse an initializer
initializer :: Parser Initializer
initializer = error "initializer: Not implemented yet"

-- | Placeholder: Parse attributes
attributes :: Parser Attributes
attributes = error "attributes: Not implemented yet"

-- | Placeholder: Parse declaration specifiers into a set
declspecs :: Parser (HashSet DeclSpecifier)
declspecs = error "declspecs: Not implemented yet"

-- | Placeholder: Parse a parameter declaration
paramdecl :: Parser ParamDecl
paramdecl = error "paramdecl: Not implemented yet"

-- | Placeholder: Parse a raw declarator
rawdeclarator :: Parser RawDeclarator
rawdeclarator = error "rawdeclarator: Not implemented yet"
