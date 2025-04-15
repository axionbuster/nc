module NC.Symbol (Symbol, Table, symnew) where

import Control.Monad.IO.Class
import Data.Coerce
import Data.Unique
import {-# SOURCE #-} NC.Parser
import NC.Symbol.Def

-- | Issue a new 'Symbol'.
symnew :: P Symbol
symnew = liftIO (coerce newUnique)
