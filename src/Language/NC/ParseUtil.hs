module Language.NC.ParseUtil where

import Control.Monad ((>>))
import Data.Functor (($>))
import FlatParse.Stateful hiding (Parser)
import Language.NC.Parse

-- * Lexing utility

-- | Parse @a@ but also make sure @q@ cannot parse (must consume span).
butnot :: Parser a -> Parser b -> Parser a
butnot p q = withSpan p \x sp -> ((inSpan sp q >> eof) `fails`) $> x
