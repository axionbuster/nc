module Language.NC.Prelude
  ( module Prelude,
    module Language.NC.Parse,
    module FlatParse.Stateful,
    Text,
    ByteString,
  )
where

import Data.ByteString (ByteString)
import Data.Text (Text)
import FlatParse.Stateful hiding (Parser)
import Language.NC.Parse
import Prelude hiding (take)
