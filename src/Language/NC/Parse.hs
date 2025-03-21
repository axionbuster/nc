-- | Parser type, error type
module Language.NC.Parse
  ( Error (..),
    PrimTypeBadWhy (..),
    Parser,
    ParserState (..),
    WithSpan (..),
    pwithspan,
    cut,
  )
where

import Control.Exception
import Data.String
import FlatParse.Stateful hiding (cut, Parser)
import Prelude

-- | An error, warning, or message of any kind.
--
-- Not all \"errors\" may be errors.
data Error
  = -- | Composite error (reverse list)
    CompositeError [Error]
  | -- | Miscellaneous programming error
    BasicError String
  | -- | Bad primitive type
    PrimTypeBadError PrimTypeBadWhy
  | -- | Unexpected end of file
    UnexpectedEOFError
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

-- | Redefinition of 'FlatParse.Stateful.cut' that intelligently
-- merges errors.
cut :: Parser a -> Error -> Parser a
cut p e = cutting p e \case
  (CompositeError l) -> \g -> CompositeError (g : l)
  f -> \g -> CompositeError [g, f]

-- | Pure \"parser\" to return a 'WithSpan'.
--
-- Argument order was readjusted to agree with 'withSpan' in "flatparse".
pwithspan :: a -> Span -> Parser (WithSpan a)
pwithspan = (pure .) . flip WithSpan
