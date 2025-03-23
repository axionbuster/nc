-- | Users should use this import rather than importing
-- individual modules or "FlatParse" modules directly.
--
-- For one thing, it redefines 'cut' from FlatParse.
module Language.NC.Prelude
  ( Parser,
    Error (..),
    PrimTypeBadWhy (..),
    ParserState (..),
    Result (..),
    Builder,
    ByteString,
    Text,
    Span (..),
    WithSpan (..),
    module Control.Monad,
    module FlatParse.Stateful,
    test_runparser0,
  )
where

import Control.Monad
import FlatParse.Stateful hiding (Parser)
import Language.NC.Internal.Prelude

-- temporary test helpers, which is why they're defined directly here.

test_runparser0 :: Parser a -> String -> IO (Result Error a)
test_runparser0 p s = do
  q <- mkstate0
  runParserIO p q 0 (strToUtf8 s)
