{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Language.NC.Internal.SBS (sbsOf) where

import Control.Monad.Primitive
import Data.ByteString.Short.Internal (ShortByteString)
import Data.ByteString.Short.Internal qualified as SBS
import FlatParse.Stateful
import GHC.Base (($))
import GHC.Exts

-- | Parse a 'ShortByteString' from the input. This operation involves
-- a copy in the 'IO' monad.
sbsOf :: ParserIO r e a -> ParserIO r e ShortByteString
sbsOf (ParserT f) = ParserT \fp !r eob s n st -> case f fp r eob s n st of
  OK# st' _ s' n' ->
    case internal (SBS.createFromPtr (Ptr s) (I# $ minusAddr# s' s)) st' of
      (# st'', sbs #) -> OK# st'' sbs s' n'
  x -> unsafeCoerce# x
