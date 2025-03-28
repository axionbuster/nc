-- | Parser type, error type
module Language.NC.Parse
  ( Parser,
    ParserState (..),
    WithSpan (..),
    Symbol (..),
    SymTab,
    Str,
    Seq (..),
    RelatedInfo (..),
    module Language.NC.Error,
    pwithspan,
    cut,
    mkstate0,
    symins,
    symget,
    symget_zr,
    symtraverse_,
    symfoldM,
    throwbasic,
  )
where

import Control.Monad
import Data.ByteString.Short
import Data.HashTable.IO qualified as H
import Data.IORef
import Data.Sequence (Seq ((:<|), (:|>)))
import FlatParse.Stateful hiding (Parser)
import Language.NC.Error
import Prelude

-- | Unpinned memory used for short strings.
type Str = ShortByteString

-- | Symbol table
type SymTab = H.CuckooHashTable Str Symbol

-- | Insert symbol
symins :: SymTab -> Str -> Symbol -> IO ()
symins = H.insert

-- | Get symbol
symget :: SymTab -> Str -> IO (Maybe Symbol)
symget = H.lookup

-- | Get symbol or throw 'mzero'
symget_zr :: SymTab -> Str -> IO Symbol
symget_zr s n =
  symget s n >>= \case
    Just m -> pure m
    _ -> mzero

-- | 'traverse_' for 'SymTab' (it\'s not an instance of 'Traversable')
symtraverse_ :: ((Str, Symbol) -> IO a) -> SymTab -> IO ()
symtraverse_ = H.mapM_

-- | 'foldM' for 'SymTab'
symfoldM :: (a -> (Str, Symbol) -> IO a) -> a -> SymTab -> IO a
symfoldM = H.foldM

-- | Symbol
data Symbol = Symbol
  deriving (Eq, Show)

-- | Parsing state, to include such things as symbol tables.
data ParserState = ParserState
  { pserrors :: IORef (Seq AnnotatedError),
    pssymbols :: SymTab
  }

-- | Create a starter state.
mkstate0 :: IO ParserState
mkstate0 = do
  e <- newIORef mempty
  h <- H.new
  pure (ParserState e h)

-- | The parser, which lives in IO.
type Parser = ParserIO ParserState Error

-- | Data with span.
data WithSpan a = WithSpan Span a
  deriving (Eq, Show)

-- | Pure \"parser\" to return a 'WithSpan'.
--
-- Argument order was readjusted to agree with 'withSpan' in "flatparse".
pwithspan :: a -> Span -> Parser (WithSpan a)
pwithspan = (pure .) . flip WithSpan

-- | Throw a 'BasicError'.
throwbasic :: String -> Parser a
throwbasic = err . BasicError
