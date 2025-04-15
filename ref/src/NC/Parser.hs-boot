module NC.Parser where

import FlatParse.Stateful

data AMessage

data PEnv

type P = ParserIO PEnv AMessage
