module Language.NC.Syn (Name, Type (..), Param (..), Dec (..), decname) where

import Language.NC.Prelude

-- saves a bit of typing
type MT x = Maybe (Name x)

-- currently no distinction, will introduce renaming and mangling.
type Name x = Text

-- | Type
data Type x
  = BasicType x (Name x)
  | PointerType x (Type x)
  | FunctionType x (Type x) [Param x]
  | -- | Unlike in C++, only one syntax exists.
    AutoType x

-- | A function parameter may have an optional type and name.
--
-- (Optional type: NC extension for definitions).
data Param x
  = Param x (Maybe (Type x)) (MT x)

-- | A declaration.
data Dec x
  = -- | storage class?, type, identifier.
    Var x (MT x) (Name x) (Name x)
  | -- | storage class?, return type?, name, parameters.
    Fun x (MT x) (Maybe (Type x)) (Name x) [Param x]

decname :: Dec x -> Name x
decname (Var _ _ _ n) = n
decname (Fun _ _ _ n _) = n
