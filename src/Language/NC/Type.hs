{-# LANGUAGE StrictData #-}

module Language.NC.Type (
  -- * Literals
  Lit (..),
  IntegerLiteral (..),
  CharacterLiteral (..),
  StringLiteral (..),

  -- * Re-export
  module Language.NC.Prim,
) where

import Data.ByteString.Lazy (LazyByteString)
import Language.NC.Prim
import Prelude

-- | A C Literal
data Lit
  = -- | Integer literal
    LitInteger IntegerLiteral
  | -- | Character constant
    LitChar CharacterLiteral
  | -- | String literal
    LitString StringLiteral
  deriving (Eq, Show)

data IntegerLiteral
  = IntegerLiteral Integer Prim
  deriving (Eq, Show)

data CharacterLiteral
  = -- | Literal character or universal character name, encoding choice delayed.
    CharacterLiteral Char Prim
  | -- | Specified in octal or hexadecimal.
    IntCharacterLiteral Integer Prim
  deriving (Eq, Show)

-- | A string literal; escape sequences have been interpreted, but
-- a NUL byte has NOT been inserted at the end. The encoding is:
--
--  - UTF-8 for single-byte-character strings
--  - UTF-16 for two-byte-character strings
--  - UTF-32 for four-byte-character strings
data StringLiteral
  = -- | Interpreted value, type of each element.
    StringLiteral LazyByteString !Prim
  deriving (Show, Eq)
