module NC.Symbol.Def (Symbol (..), Table) where

import Data.Coerce
import Data.HashTable.IO qualified as H
import Data.Hashable
import Data.Unique
import Prelude

-- | An opaque thing that identifies an object.
newtype Symbol = Symbol Unique
  deriving newtype (Eq)

-- | It will merely say @"\<symbol\>"@
instance Show Symbol where
  show _ = "<symbol>"

instance Hashable Symbol where
  hashWithSalt salt sym = hashWithSalt @Int salt (coerce hashUnique sym)

-- | We use an IO-based hash table.
type Table k v = H.BasicHashTable k v
