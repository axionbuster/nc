-- | Prelude used by certain parsing modules.
module NC.Internal.Prelude1 (
  module Control.Applicative,
  module Control.Category,
  module Control.Lens,
  module Control.Monad,
  module Control.Monad.Combinators,
  module Control.Monad.Fix,
  module Control.Monad.IO.Class,
  module Data.Bits,
  module Data.Bool,
  module Data.ByteString,
  module Data.ByteString.Lazy,
  module Data.Coerce,
  module Data.Foldable,
  module Data.Function,
  module Data.Functor,
  module Data.Hashable,
  module Data.Int,
  module Data.Maybe,
  module Data.Monoid,
  module Data.Semigroup,
  module Data.Sequence,
  module Data.Word,
  module FlatParse.Stateful,
  module FlatParse.Stateful.Parser,
  module GHC.Generics,
  module NC.Parser.Def,
  module NC.Parser.Lex,
  module NC.Type,
  module System.IO.Unsafe,
  module Text.Printf,
  module UnliftIO.IORef,
  module Unsafe.Coerce,
  module Prelude,

  -- * Some redefined functions
  traceIO,
) where

import Control.Applicative hiding (many, optional, some, (<|>))
import Control.Category ((<<<), (>>>))
import Control.Lens
import Control.Monad
import Control.Monad.Combinators hiding (
  many,
  optional,
  skipMany,
  skipSome,
  some,
  (<|>),
 )
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Bits
import Data.Bool
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (LazyByteString)
import Data.Coerce
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Hashable
import Data.Int
import Data.Maybe
import Data.Monoid
import Data.Semigroup hiding (First (..), Last (..))
import Data.Sequence (Seq ((:<|), (:|>)))
import Data.Word
import Debug.Trace qualified as Trace
import FlatParse.Stateful
import FlatParse.Stateful.Parser (pureLazy)
import GHC.Generics (Generic, Generically)
import NC.Parser.Def
import NC.Parser.Lex
import NC.Type
import System.IO.Unsafe
import Text.Printf
import UnliftIO.IORef
import Unsafe.Coerce
import Prelude hiding (take, unzip)

-- | Lifted 'Trace.traceIO'.
traceIO :: (MonadIO m) => String -> m ()
traceIO = liftIO . Trace.traceIO
