-- | Parser type, error type
module Language.NC.Internal.Parse
  ( Parser,
    ParserState (..),
    WithSpan (..),
    Symbol (..),
    Str,
    Seq (..),
    RelatedInfo (..),
    pwithspan,
    cut,
    mkstate0,
    throwbasic,
  )
where

import Data.ByteString.Short
import Data.IORef
import Data.Sequence (Seq ((:<|), (:|>)))
import FlatParse.Stateful hiding (Parser)
import Language.NC.Internal.Error
import Prelude

-- | Unpinned memory used for short strings.
type Str = ShortByteString

-- | Symbol
data Symbol = Symbol
  deriving (Eq, Show)

-- | Parsing state, to include such things as symbol tables.
data ParserState = ParserState
  { pserrors :: IORef (Seq AnnotatedError)
  }

-- | Create a starter state.
mkstate0 :: IO ParserState
mkstate0 = do
  e <- newIORef mempty
  pure (ParserState e)

-- | The parser, which lives in IO.
type Parser = ParserIO ParserState Error

-- | Data with span.
data WithSpan a = WithSpan Span a
  deriving (Eq, Show)

-- | Pure \"parser\" to return a 'WithSpan'.
--
-- Argument order was readjusted to agree with 'withSpan' in "flatparse".
pwithspan :: a -> Span -> Parser (WithSpan a)
pwithspan = (pure .) . flip WithSpan

-- | Throw a 'BasicError'.
throwbasic :: String -> Parser a
throwbasic = err . BasicError
