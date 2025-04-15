{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module: Language.NC.ExprStmt
-- Description: Expressions and statements
module Language.NC.ExprStmt (LabelInfo, Type, RecMember, Attribute) where

import Data.ByteString (ByteString)
import Data.Hashable
import GHC.Generics
import Language.NC.Type
import Prelude

data LabelInfo

type Str = ByteString

-- | An attribute
data Attribute
  = -- | optional namespace tag, then name of attr.
    Attribute !(Maybe Str) !Str
  deriving (Eq, Generic)
  deriving anyclass (Hashable)
