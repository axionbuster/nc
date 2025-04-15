{-# LANGUAGE StrictData #-}

module Language.NC.Type (
  -- * C types
  Type (..),
  UQType (..),
  Qual,
  Stor (..),
  RecMember (..),
  _qu_none,
  _qu_const,
  _qu_volatile,
  _qu_restrict,
  _qu_atomic,
  qu_none,
  qu_const,
  qu_volatile,
  qu_restrict,
  qu_atomic,

  -- * Literals
  Lit (..),
  IntegerLiteral (..),
  CharacterLiteral (..),
  StringLiteral (..),

  -- * Re-export
  module Language.NC.Prim,
) where

import Control.Lens
import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (LazyByteString)
import Data.Function
import Data.Word
import Language.NC.Prim
import NC.Symbol
import Prelude

-- | An unqualified C type
data UQType
  = UQPrim !Prim
  | UQRec !Rec
  deriving (Eq, Show)

-- | A fully-qualified C type
data Type
  = Type {_t_uqtype :: !UQType, _t_qual :: !Qual}
  deriving (Eq, Show)

-- | C qualifications
newtype Qual = Qual Word8
  deriving (Eq, Show, Bits)
  deriving (Monoid, Semigroup) via (Ior Qual)

-- | Sole qualifiers
_qu_none, _qu_const, _qu_volatile, _qu_restrict, _qu_atomic :: Qual
_qu_none = mempty
_qu_const = Qual 1
_qu_volatile = Qual 2
_qu_restrict = Qual 4
_qu_atomic = Qual 8

-- | Lens to query whether a qualifier is there
qu_none, qu_const, qu_volatile, qu_restrict, qu_atomic :: Lens' Qual Bool
qu_none = qbs _qu_none
qu_const = qbs _qu_const
qu_volatile = qbs _qu_volatile
qu_restrict = qbs _qu_restrict
qu_atomic = qbs _qu_atomic

qbs :: Qual -> Lens' Qual Bool
qbs q = lens g s
 where
  g r = r .&. q /= mempty
  s r True = r .|. q
  s r _ = r .&. complement q

-- | Formal C storage
data Stor
  = SExtern
  | SStatic
  | SRegister
  | STypedef

-- | in progres...
data RecMember
  = RecMember !Type
  deriving (Eq, Show)

-- | Is this a @struct@ or a @union@?
data RecType
  = RecStruct
  | RecUnion
  deriving (Eq, Show)

type Str = ByteString

-- | Body of a @struct@ or @union@
data RecBody
  = RecBody
  { -- | All members in order of declaration
    _recb_membs :: ![RecMember],
    -- | The same members, but from strings instead.
    _recb_names :: !(Table Str RecMember)
  }

instance Show RecBody where
  show _ = "RecBody"

data Rec
  = Rec {_rec_sym :: Symbol, _rec_t :: !RecType, _rec_body :: !RecBody}

instance Eq Rec where
  (==) = (==) `on` _rec_sym

instance Show Rec where
  show _ = "Rec"

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
