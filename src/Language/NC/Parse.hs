-- | Parser type, error type
module Language.NC.Parse
  ( Error (..),
    PrimTypeBadWhy (..),
    Parser,
    ParserState (..),
    WithSpan (..),
    pwithspan,
  )
where

import Control.Exception
import Data.String
import FlatParse.Stateful hiding (Parser)
import Prelude

-- | An error, warning, or message of any kind.
--
-- Not all \"errors\" may be errors.
data Error
  = -- | Composite error
    CompositeError [Error]
  | -- | Miscellaneous programming error
    BasicError String
  | -- | Bad primitive type
    PrimTypeBadError PrimTypeBadWhy
  | -- | Internal error
    InternalError String
  deriving (Eq, Show)

-- | Why a primitive type couldn't be parsed, or is incorrect.
data PrimTypeBadWhy
  = -- | @(un)signed long double@
    BecauseSignInLongDouble
  | -- | Type cannot be given signedness
    BecauseSignNotMeaningful
  deriving (Eq, Show)

instance Exception Error

instance IsString Error where
  fromString = BasicError

-- | Parsing state, to include such things as symbol tables.
data ParserState = ParserState

-- | The parser, which lives in IO.
type Parser = ParserIO ParserState Error

-- | Data with span
data WithSpan a = WithSpan Span a
  deriving (Eq, Show)

-- | Pure \"parser\" to return a 'WithSpan'.
--
-- Argument order was readjusted to agree with 'withSpan' in "flatparse".
pwithspan :: a -> Span -> Parser (WithSpan a)
pwithspan = (pure .) . flip WithSpan
