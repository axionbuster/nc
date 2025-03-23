module Language.NC.Dec where

import Language.NC.CTypes
import Language.NC.Internal.Prelude

-- | A declaration. I'm not including attributes, yet.
data Dec = Dec Linkage StorageClass QualifiedType Name

-- | Separate linkage type if needed
data Linkage = Internal | External | NoLinkage
  deriving (Eq, Show)
