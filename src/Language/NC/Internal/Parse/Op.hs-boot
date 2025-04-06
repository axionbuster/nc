module Language.NC.Internal.Parse.Op where

import Language.NC.Internal.Prelude hiding (assign)

-- | Parse an expression.
expr_ :: Parser Expr

assign :: Parser Expr
