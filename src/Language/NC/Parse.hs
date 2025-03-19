-- | Parser type, error type
module Language.NC.Parse (Error (..), Parser, ParserState (..)) where

import Control.Exception
import Data.String
import FlatParse.Stateful hiding (Parser)
import Prelude

data Error = BasicError String

instance Exception Error

instance Show Error where
  show (BasicError e) = e

instance IsString Error where
  fromString = BasicError

data ParserState = ParserState

type Parser = ParserIO ParserState [Error]
