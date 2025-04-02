{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

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
  Declaration(..),
  Declarator(..),
  Initializer(..),
  
  -- * Declaration Constructors
  -- These are exported through Declaration(..)
  
  -- * Components
  ParamDecl(..),
  Attributes(..),
  Attribute(..),
  AttributeValue(..),
  
  -- * Optics
  -- | Lenses for accessing and modifying declaration components
  declnam, decltyp, declstor, declattr, declparam,
  declret, declinit, attrnam, attrval, attrvals,
  
  -- * Parsers
  declaration,
  declarators,
  initializer,
  attributes,
) where

import Control.Lens
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Language.NC.Internal.Lex.Lex
import Language.NC.Internal.Lex.Op (Expr)
import Language.NC.Internal.Lex.Type
import Language.NC.Internal.Prelude

-- | The primary declaration type representing all C declarations.
-- This provides a unified interface while supporting specific declaration kinds.
data Declaration
  = -- | Variable or object declaration
    VarDeclaration
      { _declnam :: Symbol                  -- ^ Name of the declared variable
      , _decltyp :: Type                    -- ^ Type of the variable
      , _declstor :: StorageClass           -- ^ Storage class (static, extern, etc.)
      , _declattr :: Attributes             -- ^ Attributes applied to the declaration
      , _declinit :: Maybe Initializer      -- ^ Optional initializer
      }
  | -- | Type definition (typedef)
    TypeDeclaration
      { _declnam :: Symbol                  -- ^ Name of the type alias
      , _decltyp :: Type                    -- ^ The aliased type
      , _declattr :: Attributes             -- ^ Attributes applied to the type
      }
  | -- | Function declaration or definition
    FuncDeclaration
      { _declnam :: Symbol                  -- ^ Function name
      , _declparam :: [ParamDecl]           -- ^ Function parameters
      , _declret :: Type                    -- ^ Return type
      , _declstor :: StorageClass           -- ^ Storage class
      , _declattr :: Attributes             -- ^ Function attributes
      }
  | -- | Static assertion
    StaticAssertDeclaration
      { _asrtexpr :: Expr                   -- ^ The assertion expression
      , _asrtmsg :: Maybe Str               -- ^ Optional message
      }
  | -- | Standalone attribute declaration
    AttributeOnlyDeclaration
      { _declattr :: Attributes             -- ^ The attributes
      }
  deriving (Eq, Show)

-- | A parameter declaration within a function
data ParamDecl = ParamDecl
  { _paramnam :: Maybe Symbol               -- ^ Parameter name (optional)
  , _paramtyp :: Type                       -- ^ Parameter type
  , _paramattr :: Attributes                -- ^ Parameter attributes
  }
  deriving (Eq, Show)

-- | A collection of attributes applied to a declaration
newtype Attributes = Attributes
  { _attrvals :: Map Symbol AttributeValue  -- ^ Map of attribute names to values
  }
  deriving (Eq, Show)

-- | An individual attribute with name and value
data Attribute = Attribute
  { _attrnam :: Symbol                      -- ^ Attribute name
  , _attrval :: AttributeValue              -- ^ Attribute value
  }
  deriving (Eq, Show)

-- | The value of an attribute
data AttributeValue
  = AttrFlag                                -- ^ Flag-only attribute (no value)
  | AttrStr Str                             -- ^ String value
  | AttrExpr Expr                           -- ^ Expression value
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
    FuncDeclarator Declarator [ParamDecl] Bool Attributes  -- Bool indicates variadic
  deriving (Eq, Show)

-- | Initializer for variables and objects
data Initializer
  = -- | Simple expression initializer
    ExprInit Expr
  | -- | Braced initializer list 
    ListInit [DesignatedInit]
  deriving (Eq, Show)

-- | An initializer with optional designator
data DesignatedInit = DesignatedInit
  { _designator :: Maybe Designator
  , _initValue :: Initializer
  }
  deriving (Eq, Show)

-- | Designator for specifying which member to initialize
data Designator
  = -- | Array index designator [index]
    ArrayDesignator Expr
  | -- | Member designator .member
    MemberDesignator Symbol
  deriving (Eq, Show)

-- | Intermediate representation for flexible parsing
data DeclComponents = DeclComponents
  { _dcSpecs :: Set DeclSpecifier      -- ^ Declaration specifiers in any order
  , _dcDeclarators :: [RawDeclarator]  -- ^ Raw declarators before processing
  , _dcAttributes :: [Attribute]       -- ^ Attributes in raw form
  }

-- | Raw specifier collected during parsing
data DeclSpecifier 
  = DSStorageClass StorageClass
  | DSType Type
  | DSTypeQual TypeQual
  | DSFuncSpec FuncSpec
  deriving (Eq, Show)

-- | Wrapper for Type with Ord instance for use in Sets
newtype OrderedType = OrderedType Type
  deriving (Eq, Show)
  
-- | Ord instance for DeclSpecifier
instance Ord DeclSpecifier where
  compare (DSStorageClass a) (DSStorageClass b) = compare a b
  compare (DSType a) (DSType b) = 
    -- For now, compare by string representation
    -- This is not ideal but works for immediate needs
    compare (show a) (show b)
  compare (DSTypeQual a) (DSTypeQual b) = compare a b
  compare (DSFuncSpec a) (DSFuncSpec b) = compare a b
  
  -- Constructor ordering (for different constructors)
  compare (DSStorageClass _) _ = LT
  compare _ (DSStorageClass _) = GT
  compare (DSType _) _ = LT
  compare _ (DSType _) = GT
  compare (DSTypeQual _) _ = LT
  compare _ (DSTypeQual _) = GT

-- | Raw declarator before semantic processing
data RawDeclarator = RawDeclarator
  { _rdName :: Maybe Symbol
  , _rdDerivers :: [TypeDeriver]
  , _rdAttrs :: [Attribute]
  }
  deriving (Eq, Show)

-- | Type derivation operations
data TypeDeriver
  = TDPointer TypeQual
  | TDArray (Maybe Expr)
  | TDFunction [ParamDecl] Bool  -- Bool indicates variadic
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

-- Pattern synonyms removed for simplicity

-- | Converts set of specifiers to appropriate type and storage information
resolveSpecifiers :: Set DeclSpecifier -> (Type, StorageClass, FuncSpec)
resolveSpecifiers specs = error "resolveSpecifiers: Not implemented yet"

-- | Converts raw declarator to processed Declarator
processDeclarator :: RawDeclarator -> Type -> Declarator
processDeclarator raw baseType = error "processDeclarator: Not implemented yet"

-- | Creates final declaration from components
makeDeclaration :: DeclComponents -> Either [Error] [Declaration]
makeDeclaration comps = error "makeDeclaration: Not implemented yet"

-- | Empty attributes container
emptyAttrs :: Attributes
emptyAttrs = Attributes Map.empty

-- | Adds an attribute to an Attributes container
addattr :: Attribute -> Attributes -> Attributes
addattr (Attribute name val) (Attributes attrs) = 
  Attributes $ Map.insert name val attrs

-- | Creates an attribute from name and value
mkattr :: Symbol -> AttributeValue -> Attribute
mkattr = Attribute

-- | Creates a flag attribute (no value)
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
declspecs :: Parser (Set DeclSpecifier)
declspecs = error "declspecs: Not implemented yet"

-- | Placeholder: Parse a parameter declaration
paramdecl :: Parser ParamDecl
paramdecl = error "paramdecl: Not implemented yet"

-- | Placeholder: Parse a raw declarator
rawdeclarator :: Parser RawDeclarator
rawdeclarator = error "rawdeclarator: Not implemented yet"