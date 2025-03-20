module Language.NC.Dec where

import Language.NC.CTypes
import Language.NC.Prelude

-- | A declaration. I'm not including attributes, yet.
data Dec a = Dec (DecX a) Linkage StorageClass (QualifiedType a) (Name a)

-- | Separate linkage type if needed
data Linkage = Internal | External | NoLinkage
  deriving (Eq, Show)

type instance DecX () = ()

type family DecX a
