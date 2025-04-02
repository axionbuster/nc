module Language.NC.Internal.Lex.Op where

import Language.NC.Internal.Prelude

data Expr

instance Eq Expr
instance Show Expr

assign :: Parser Expr
