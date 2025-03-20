module Language.NC.Prelude
  ( module Control.Monad,
    module Control.Monad.Fix,
    module Prelude,
    module Language.NC.Parse,
    module FlatParse.Stateful,
    module Data.Char,
    module Data.Dynamic,
    module Data.Function,
    module Data.Functor,
    module Data.Int,
    module Data.Word,
    module GHC.Float,
    module GHC.Generics,
    Builder,
    ByteString,
    Text,
    builder2bs,
    fromStrict,
    toStrict,
  )
where

import Control.Monad
import Control.Monad.Fix
import Data.ByteString (ByteString, fromStrict, toStrict)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BB
import Data.Char hiding (isDigit)
import Data.Dynamic
import Data.Function
import Data.Functor
import Data.Int
import Data.Text (Text)
import Data.Word
import FlatParse.Stateful hiding (Parser)
import GHC.Float
import GHC.Generics
import Language.NC.Parse
import Prelude hiding (take, unzip)

builder2bs :: Builder -> ByteString
builder2bs = toStrict . BB.toLazyByteString
