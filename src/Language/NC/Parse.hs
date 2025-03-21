-- | Parser type, error type
module Language.NC.Parse
  ( Error (..),
    PrimTypeBadWhy (..),
    Parser,
    ParserState (..),
  )
where

import Control.Exception
import Data.String
import FlatParse.Stateful hiding (Parser)
import Prelude

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

data PrimTypeBadWhy
  = -- | @(un)signed long double@
    BecauseSignInLongDouble
  | -- | Type cannot be given signedness
    BecauseSignNotMeaningful
  deriving (Eq, Show)

instance Exception Error

instance IsString Error where
  fromString = BasicError

data ParserState = ParserState

type Parser = ParserIO ParserState Error
