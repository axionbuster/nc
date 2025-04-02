{-# LANGUAGE BangPatterns #-}

-- | Symbol definitions and symbol table implementation for the NC parser.
-- This module handles unique symbol creation, lookup, and scope management.
module Language.NC.Internal.Symbol (
  -- * Types
  Symbol (..),
  SymbolInfo (..),
  Str,
  SymbolTable,
  ScopeStack,
  ScopeInfo,

  -- * Operations
  newsymtable,
  newUnique,
  enterscope,
  exitscope,
  symdefine,
  symlookup,
  symname,
  sympresent,
  symtype,
) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.HashTable.IO qualified as H
import Data.Hashable (Hashable (..))
import Data.IORef
import Data.Unique hiding (newUnique)
import Data.Unique qualified as U
import FlatParse.Stateful (Span (..), err)
import Language.NC.Internal.Error
import {-# SOURCE #-} Language.NC.Internal.Parse (Parser, cmpspans)
import Language.NC.Internal.PrimTypes qualified as PT
import Prelude

-- | Memory used for strings. We take advantage of strict 'ByteString' sharing
-- done by "flatparse".
type Str = ByteString

-- | Symbol (without the actual name).
-- Wrapper around 'Unique' with proper instances.
newtype Symbol = Symbol Unique
  deriving (Eq, Ord)

instance Show Symbol where
  show _ = "(anonymous)"

-- Make Symbol hashable for use in hash tables
instance Hashable Symbol where
  hashWithSalt s (Symbol u) = hashWithSalt s (hashUnique u)

-- | Information about a symbol.
-- This includes the symbol's name, type, and location.
data SymbolInfo = SymbolInfo
  { -- | The symbol's name.
    si_name :: !Str,
    -- | The symbol's type.
    si_type :: !PT.PrimType,
    -- | The symbol's location in the source code.
    --   In rare occasions this may be a bogus location, that is,
    --   0:0, if the symbol was created in thin air for various reasons.
    si_loc :: !Span
  }
  deriving (Eq, Show)

instance Ord SymbolInfo where
  SymbolInfo n1 t1 s1 `compare` SymbolInfo n2 t2 s2 =
    compare n1 n2 <> compare t1 t2 <> cmpspans s1 s2
  {-# INLINE compare #-}

-- | Create a new unique symbol.
newUnique :: IO Symbol
newUnique = Symbol <$> U.newUnique

-- | Fast IO-based hash table for global symbol information
type FastTable k v = H.BasicHashTable k v

-- | Information stored about a symbol within a scope
data ScopeInfo = ScopeInfo
  { -- | Symbol's name
    scope_name :: !Str,
    -- | Symbol's type
    scope_type :: !PT.PrimType
    -- Add other properties as needed
  }

-- | Stack of scopes for maintaining lexical environments
newtype ScopeStack = ScopeStack
  { -- | Chain of scope tables, innermost first
    unscopestack :: [FastTable Str Symbol]
  }

-- | Complete symbol table consisting of global and scoped information
data SymbolTable = SymbolTable
  { -- | Fast global unique -> identifier table
    symtab_names :: !(FastTable Symbol Str),
    -- | Current scope stack
    symtab_scopes :: !(IORef ScopeStack),
    -- | Type definitions table
    symtab_types :: !(FastTable Symbol PT.PrimType)
  }

-- | Create a new empty symbol table
newsymtable :: IO SymbolTable
newsymtable = do
  names <- H.new
  globalScope <- H.new
  scopes <- newIORef (ScopeStack [globalScope])
  types <- H.new
  pure $ SymbolTable names scopes types

-- | Enter a new scope
enterscope :: SymbolTable -> IO ()
enterscope st = do
  newScope <- H.new
  modifyIORef' (symtab_scopes st) $ \(ScopeStack scopes) ->
    ScopeStack (newScope : scopes)

-- | Exit the current scope
exitscope :: SymbolTable -> IO ()
exitscope st = do
  modifyIORef' (symtab_scopes st) $ \(ScopeStack scopes) ->
    case scopes of
      [] -> ScopeStack [] -- Should never happen if used correctly
      (_ : rest) -> ScopeStack rest

-- | Define a symbol in the current scope with error handling
symdefine :: SymbolTable -> Str -> PT.PrimType -> Span -> Parser Symbol
symdefine st name typ _loc = do
  sym <- liftIO newUnique
  scopes <- liftIO $ readIORef (symtab_scopes st)
  case scopes of
    ScopeStack [] -> err $ InternalError "symdefine: no scope available"
    ScopeStack (currentScope : _) -> liftIO do
      H.insert currentScope name sym
      H.insert (symtab_names st) sym name
      H.insert (symtab_types st) sym typ
      pure sym

-- | Check if a symbol exists in the current scope
sympresent :: SymbolTable -> Str -> IO (Maybe Symbol)
sympresent st name = do
  ScopeStack scopes <- readIORef (symtab_scopes st)
  case scopes of
    [] -> pure Nothing
    (currentScope : _) -> H.lookup currentScope name

-- | Get the type of a symbol
symtype :: SymbolTable -> Symbol -> IO (Maybe PT.PrimType)
symtype st sym = H.lookup (symtab_types st) sym

-- | Look up a symbol by name in scopes
symlookup :: SymbolTable -> Str -> IO (Maybe (Symbol, PT.PrimType))
symlookup st name = do
  ScopeStack scopes <- readIORef (symtab_scopes st)
  -- Search from innermost scope outward
  findsym scopes
 where
  findsym [] = pure Nothing
  findsym (scope : rest) = do
    result <- H.lookup scope name
    case result of
      Just sym -> do
        typMaybe <- H.lookup (symtab_types st) sym
        case typMaybe of
          Just typ -> pure $ Just (sym, typ)
          Nothing -> pure Nothing -- Should never happen if tables consistent
      Nothing -> findsym rest

-- | Get name for a symbol if it exists
symname :: SymbolTable -> Symbol -> IO (Maybe Str)
symname st sym = H.lookup (symtab_names st) sym
