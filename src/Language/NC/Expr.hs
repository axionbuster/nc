module Language.NC.Expr (Expr) where

import Prelude (Eq, Show)

-- | Placeholder for expression; used for dependent typing in variable
-- sized arrays and for enumerations. Right now, you can't make an instance of
-- type, so any constructor that requires this type also can't be constructed.
data Expr
  deriving (Eq, Show)
