module Language.NC.Internal.Parse where

import Language.NC.Internal.Error (Error)
import FlatParse.Stateful (Span, ParserIO)
import Prelude (Ordering)

data ParserState
type Parser = FlatParse.Stateful.ParserIO ParserState Error
cmpspans :: Span -> Span -> Ordering