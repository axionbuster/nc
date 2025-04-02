module Language.NC.Internal.Lex.Type where

import Language.NC.Internal.Prelude

parsetype :: Parser Type

-- Core types needed for circular dependencies
data Type
data BaseType
data TypeQual
data StorageClass
data FuncSpec
data Alignment
