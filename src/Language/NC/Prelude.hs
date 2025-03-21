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
    test_runparser0,
  )
where

import Language.NC.Internal.Prelude

-- temporary test helpers, which is why they're defined directly here.

test_runparser0 :: Parser a -> String -> IO (Result Error a)
test_runparser0 p s = runParserIO p ParserState 0 (strToUtf8 s)
