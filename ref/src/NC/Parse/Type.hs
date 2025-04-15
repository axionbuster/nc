module NC.Parse.Type (
  -- * Types
  SQ,
  SQBag,
  SQT,
) where

import Data.Bits
import Data.Semigroup
import Data.Word
import NC.Parser
import Prelude

newtype SQ = SQ Word64
  deriving (Show, Eq, Bits)
  deriving (Monoid, Semigroup) via (Ior SQ)

-- | Result of collecting specifier-qualifier tokens.
data SQBag
  = -- | Errors and then final specifier-qualifier set
    SQBag ![String] !SQ
  deriving (Show, Eq)

newtype SQT = SQT (SQBag -> SQBag)
  deriving (Monoid, Semigroup) via (Dual (Endo SQBag))
