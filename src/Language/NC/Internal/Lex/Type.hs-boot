module Language.NC.Internal.Lex.Type where

import Language.NC.Internal.Prelude

parsetype :: Parser Type

-- Core types needed for circular dependencies
data Type
instance Eq Type
instance Show Type
instance Ord Type

data BaseType
instance Eq BaseType
instance Show BaseType

data TypeQual
instance Eq TypeQual
instance Show TypeQual

data StorageClass
instance Eq StorageClass
instance Show StorageClass

data FuncSpec
instance Eq FuncSpec
instance Show FuncSpec

data Alignment
instance Eq Alignment
instance Show Alignment
