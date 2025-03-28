{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Language.NC.Parse.Decl where

import Language.NC.Experiment.Types
import Language.NC.Internal.Prelude
import Language.NC.Lex2
import Language.NC.Parse.Expr

-- | A declaration is a statement that introduces a new name
--   or identifier into the program. It can be a type declaration,
--   a type alias declaration, or a data declaration.
data Decl
  = -- | @struct@, @union@, @enum@, or our new @union struct@
    TypeDecl Type
  | -- | @typedef@
    TypeAliasDecl Name Type
  | -- | Object or function declaration
    DataDecl
      { -- | storage class specifier
        declstor :: Maybe Storage,
        -- | qualified type
        decltype :: Type,
        -- | exactly one name or unique identifier
        --   this is even for anonymous declarations;
        --   we give them internal unique identifiers
        declname :: Name,
        -- | optional initializer
        declinit :: Maybe Expr
      }

_decltype :: Lens' Decl Type
_decltype f (TypeDecl x) = TypeDecl <$> f x
_decltype f (TypeAliasDecl n x) = TypeAliasDecl n <$> f x
_decltype f (DataDecl a b c d) = (\x -> DataDecl a x c d) <$> f b

_typeattr :: Lens' Type [Attribute]
_typeattr f t = (\x -> t {typeattr = x}) <$> f (typeattr t)

_decltypeattr :: Lens' Decl [Attribute]
_decltypeattr = _decltype . _typeattr

-- C's declaration syntax is highly complex; so,
-- it warrants a two-step parsing approach: first, a concrete
-- syntax tree (CST) is computed, and then it's analyzed and
-- translated into AST nodes.

data RawSpec
  = RSStorage Storage
  | RSType Pretype
  | RSQual Qualifier
  | RSAttr Attribute
  deriving (Show, Eq)

data DirDector
  = RDIdent Name
  | -- | purely for syntax
    RDParen RawDector
  deriving (Show, Eq)

data RawDector
  = RawDector
  { dtrptrs :: [PointerInfo],
    dtrdirect :: DirDector,
    dtrsfxs :: [DeclSufx],
    dtrinit :: Maybe Expr
  }
  deriving (Show, Eq)

data DeclSufx
  = DSxArray [Qualifier] (Maybe Expr)
  | DSxFunc [Parameter]
  deriving (Show, Eq)

data PointerInfo
  = PointerInfo [Qualifier]
  deriving (Show, Eq)

data Parameter
  = Parameter
  { parattrs :: [Attribute],
    partype :: Type,
    parname :: Maybe Name
  }
  deriving (Show, Eq)

data RawDecl = RawDecl [RawSpec] [RawDector]
  deriving (Show, Eq)

-- raw parse a declaration
rawdecl = do
  sps <- many (lx specifier)
  dcs <- lx dector `sepBy` comma
  semicolon
  pure $ RawDecl sps dcs

analyzedecls (RawDecl sps dcs) = do
  bas <- xbasetype sps
  sto <- xstorage sps
  forM dcs $ mkdecl bas sto

specifier =
  choice
    [ RSStorage <$> storage,
      RSType <$> type_,
      RSQual <$> qual,
      RSAttr <$> attr
    ]

dector = do
  RawDector
    <$> many (lx pointer)
    <*> (lx directdector)
    <*> many (lx dectorsfx)
    <*> optional (equal >> initializer)

mkdecl bas sto (RawDector ptrs dir sfxs init) = do
  nam <- case dir of
    RDIdent n -> Right n
    RDParen d -> xname d
  let withptrs = foldr (\p t -> Type (ptrQuals p) [] (PPointer t)) bas ptrs
  completetype <- foldM applysfx withptrs sfxs
  pure $ DataDecl sto completetype nam init

xbasetype = error "xbasetype"

xstorage = error "xstorage"

initializer = error "initializer (returns Expr) not impl'd."

{- the following is a suggestion by Claude:

Claude doesn't know all the types or whatever existing
machinerey that's already here but here you go:

-- Parse a specifier (storage class, type specifier, or qualifier)
specifier :: Parser RawSpec
specifier = choice
  [ StorageSpec <$> storageSpecifier
  , TypeSpec <$> typeSpecifier
  , QualSpec <$> typeQualifier
  , AttrSpec <$> attributeSpecifier
  ]

-- Parse a declarator (with optional pointer, arrays, function parameters)
dector :: Parser RawDector
dector = do
  ptrs <- many pointer
  direct <- directDector
  suffixes <- many declaratorSuffix
  initializer <- optional (equals *> initializerP)
  return $ RawDector ptrs direct suffixes initializer

-- Extract base type from raw specifiers
xbasetype :: [RawSpec] -> Either String Type
xbasetype specs = do
  -- Extract all type specifiers
  let typeSpecs = [t | TypeSpec t <- specs]
  let quals = [q | QualSpec q <- specs]
  let attrs = [a | AttrSpec a <- specs]
  
  -- Combine type specifiers to form base type
  baseType <- case typeSpecs of
    [] -> Left "No type specifier provided"
    [Void] -> Right $ Type Void [] attrs
    [Char] -> Right $ Type Char [] attrs
    [Short] -> Right $ Type Short [] attrs
    [Int] -> Right $ Type Int [] attrs
    [Long] -> Right $ Type Long [] attrs
    [Long, Long] -> Right $ Type LongLong [] attrs
    [Short, Int] -> Right $ Type Short [] attrs
    [Long, Int] -> Right $ Type Long [] attrs
    [Long, Long, Int] -> Right $ Type LongLong [] attrs
    [Float] -> Right $ Type Float [] attrs
    [Double] -> Right $ Type Double [] attrs
    [Long, Double] -> Right $ Type LongDouble [] attrs
    [Bool] -> Right $ Type Bool [] attrs
    [StructUnionType su] -> Right $ Type (Struct su) [] attrs
    [EnumType e] -> Right $ Type (Enum e) [] attrs
    [TypedefName n] -> Right $ Type (TypedefType n) [] attrs
    _ -> Left $ "Invalid combination of type specifiers: " ++ show typeSpecs
  
  -- Apply qualifiers to the base type
  return $ applyQualifiers quals baseType

-- Extract storage class from raw specifiers
xstorage :: [RawSpec] -> Either String (Maybe Storage)
xstorage specs = do
  let storageSpecs = [s | StorageSpec s <- specs]
  case storageSpecs of
    [] -> Right Nothing
    [s] -> Right (Just s)
    _ -> Left $ "Multiple storage class specifiers: " ++ show storageSpecs

-- Construct a declaration from base type and a raw declarator
mkdecl :: Type -> Maybe Storage -> RawDector -> Either String Decl
mkdecl baseType storage (RawDector ptrs direct suffixes initializer) = do
  -- Get name from direct declarator
  name <- case direct of
    IdentDector n -> Right n
    ParenDector d -> extractName d
  
  -- Apply pointer, array, and function modifiers to build the complete type
  let withPtrs = foldr (\p t -> Type (Pointer t) (ptrQuals p) []) baseType ptrs
  completeType <- foldM applySuffix withPtrs suffixes
  
  -- Construct the declaration
  return $ DataDecl storage completeType name initializer

-- Utility functions for the above

-- Apply a declarator suffix (array or function) to a type
applySuffix :: Type -> DeclaratorSuffix -> Either String Type
applySuffix baseType suffix = case suffix of
  ArraySuffix quals size -> 
    Right $ Type (Array baseType size) quals []
  FunctionSuffix params ->
    Right $ Type (Function baseType params) [] []

-- Extract name from a nested declarator
extractName :: RawDector -> Either String Name
extractName (RawDector _ (IdentDector n) _ _) = Right n
extractName (RawDector _ (ParenDector d) _ _) = extractName d
extractName _ = Left "Could not extract name from declarator"

-- Parser for pointer declarator
pointer :: Parser PointerInfo
pointer = do
  asterisk
  quals <- many typeQualifier
  return $ PointerInfo quals

-- Parser for direct declarator (identifier or parenthesized)
directDector :: Parser DirectDector
directDector = choice
  [ IdentDector <$> identifier
  , ParenDector <$> (lparen *> dector <* rparen)
  ]

-- Parser for declarator suffixes (array or function)
declaratorSuffix :: Parser DeclaratorSuffix
declaratorSuffix = choice
  [ arrayDectorSuffix
  , functionDectorSuffix
  ]

-- Parser for array declarator suffix
arrayDectorSuffix :: Parser DeclaratorSuffix
arrayDectorSuffix = do
  lbracket
  -- Optional static and qualifiers
  quals <- many typeQualifier
  size <- optional assignmentExpr
  rbracket
  return $ ArraySuffix quals size

-- Parser for function declarator suffix
functionDectorSuffix :: Parser DeclaratorSuffix
functionDectorSuffix = do
  lparen
  params <- parameterList
  rparen
  return $ FunctionSuffix params

-- Additional data types needed

data RawSpec
  = StorageSpec Storage
  | TypeSpec TypeSpecifier
  | QualSpec TypeQualifier
  | AttrSpec Attribute
  deriving (Show)

data DirectDector
  = IdentDector Name
  | ParenDector RawDector
  deriving (Show)

data DeclaratorSuffix
  = ArraySuffix [TypeQualifier] (Maybe Expr)
  | FunctionSuffix [Parameter]
  deriving (Show)

data PointerInfo = PointerInfo [TypeQualifier]
  deriving (Show)

data RawDector = RawDector 
  { dectorPtrs :: [PointerInfo]
  , dectorDirect :: DirectDector
  , dectorSuffixes :: [DeclaratorSuffix]
  , dectorInit :: Maybe Expr
  }
  deriving (Show)

-}
