-- |
-- Module      : NC.Type.Prim
-- Description : Primitive C language types and their efficient encoding
-- Copyright   : (c) 2024-2025
-- License     : BSD-3-Clause
-- Maintainer  : axionbuster@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module defines the representation of primitive C types in the NC
-- compiler.
--
-- * Primitive types include standard C types like int, char, long, float, etc.
-- * The bit layout encodes information about signedness, size, and special
--   categories
-- * Special provision is made for C23's _BitInt(N) type with variable bit width
--
-- This module also includes support for parsing @\_Complex \_Decimal...@ types,
-- which do not exist in standard C. However, it allows expressing the types
-- which can be used for error reporting and perhaps more.
module NC.Type.Prim (
  -- * Types
  Prim (
    PInt,
    PBool,
    PSUChar,
    PNSChar,
    PShort,
    PLong,
    PLongLong,
    PBitInt,
    PFloat,
    PVoid,
    PNullptr,
    PrimInt,
    PrimUInt,
    PrimBool,
    PrimSChar,
    PrimUChar,
    PrimChar,
    PrimShort,
    PrimUShort,
    PrimLong,
    PrimULong,
    PrimLongLong,
    PrimULongLong,
    PrimBitInt,
    PrimUBitInt,
    PrimFloat,
    PrimDouble,
    PrimLongDouble,
    PrimCFloat,
    PrimCDouble,
    PrimCLongDouble,
    PrimVoid,
    PrimNullptr,
    PrimD32,
    PrimD64,
    PrimD128,
    PrimCD32,
    PrimCD64,
    PrimCD128
  ),
  BitSize,
  -- Pattern synonyms that replace PrimInfo
  PrimSignedInteger (..),
  SIntegerCategory (..),
  Sign (..),
  Complex (..),
  FloatCategory (..),

  -- * Lenses
  pi_si,
) where

import Control.Lens
import Data.Word
import Prelude

-- | Bit sizes for stuff.
type BitSize = Word16

-- | Primitive, non-derived types. This is implemented as a simple enum
-- for maintainability.
data Prim
  = PInt !Sign
  | PBool
  | PSUChar !Sign
  | PNSChar
  | PShort !Sign
  | PLong !Sign
  | PLongLong !Sign
  | PBitInt !Sign !BitSize
  | PFloat !Complex !FloatCategory
  | PVoid
  | PNullptr
  deriving (Eq, Show)

-- | Is an integer type signed?
data Sign = Signed | Unsigned
  deriving (Eq, Show)

-- | Is a floating-point type real or complex?
data Complex = CxReal | CxComplex
  deriving (Eq, Show)

-- | @float@, @double@, etc.
data FloatCategory
  = FCFloat
  | FCDouble
  | FCLongDouble
  | FCDecimal32
  | FCDecimal64
  | FCDecimal128
  deriving (Eq, Show)

-- | Pattern synonyms that replace the raw 'Prim' constructors for convenience.
pattern PrimInt, PrimUInt :: Prim

-- | C @int@
pattern PrimInt = PInt Signed

-- | C @unsigned int@
pattern PrimUInt = PInt Unsigned

-- | C @\_Bool@
pattern PrimBool :: Prim
pattern PrimBool = PBool

pattern PrimSChar, PrimUChar, PrimChar :: Prim

-- | C @signed char@
pattern PrimSChar = PSUChar Signed

-- | C @unsigned char@
pattern PrimUChar = PSUChar Unsigned

-- | C @char@
pattern PrimChar = PNSChar

pattern PrimShort, PrimUShort :: Prim

-- | C @short@
pattern PrimShort = PShort Signed

-- | C @unsigned short@
pattern PrimUShort = PShort Unsigned

-- | C @long@
pattern PrimLong, PrimULong :: Prim

-- | C @long@
pattern PrimLong = PLong Signed

-- | C @unsigned long@
pattern PrimULong = PLong Unsigned

pattern PrimLongLong, PrimULongLong :: Prim

-- | C @long long@
pattern PrimLongLong = PLongLong Signed

-- | C @unsigned long long@
pattern PrimULongLong = PLongLong Unsigned

pattern PrimBitInt, PrimUBitInt :: BitSize -> Prim

-- | C @\_BitInt(N)@
pattern PrimBitInt w = PBitInt Signed w

-- | C @unsigned \_BitInt(N)@
pattern PrimUBitInt w = PBitInt Unsigned w

pattern PrimFloat, PrimDouble, PrimLongDouble :: Prim

-- | C @float@
pattern PrimFloat = PFloat CxReal FCFloat

-- | C @double@
pattern PrimDouble = PFloat CxReal FCDouble

-- | C @long double@
pattern PrimLongDouble = PFloat CxReal FCLongDouble

pattern PrimCFloat, PrimCDouble, PrimCLongDouble :: Prim

-- | C @_Complex float@
pattern PrimCFloat = PFloat CxComplex FCFloat

-- | C @_Complex double@
pattern PrimCDouble = PFloat CxComplex FCDouble

-- | C @_Complex long double@
pattern PrimCLongDouble = PFloat CxComplex FCLongDouble

-- | @void@, sort of corresponds to 'Data.Void.Void'
pattern PrimVoid :: Prim
pattern PrimVoid = PVoid

-- | @nullptr\_t@, sort of corresponds to @()@
pattern PrimNullptr :: Prim
pattern PrimNullptr = PNullptr

pattern PrimD32, PrimD64, PrimD128 :: Prim

-- | C @_Decimal32@
pattern PrimD32 = PFloat CxReal FCDecimal32

-- | C @_Decimal64@
pattern PrimD64 = PFloat CxReal FCDecimal64

-- | C @_Decimal128@
pattern PrimD128 = PFloat CxReal FCDecimal128

pattern PrimCD32, PrimCD64, PrimCD128 :: Prim

-- | C @_Complex _Decimal32@, invalid type.
pattern PrimCD32 = PFloat CxReal FCDecimal32

-- | C @_Complex _Decimal64@, invalid type.
pattern PrimCD64 = PFloat CxReal FCDecimal64

-- | C @_Complex _Decimal128@, invalid type.
pattern PrimCD128 = PFloat CxReal FCDecimal128

{-# COMPLETE
  PrimInt,
  PrimUInt,
  PrimBool,
  PrimSChar,
  PrimUChar,
  PrimChar,
  PrimShort,
  PrimUShort,
  PrimLong,
  PrimULong,
  PrimLongLong,
  PrimULongLong,
  PrimBitInt,
  PrimUBitInt,
  PrimFloat,
  PrimDouble,
  PrimLongDouble,
  PrimCFloat,
  PrimCDouble,
  PrimCLongDouble,
  PrimD32,
  PrimD64,
  PrimD128,
  -- right now i'm not including the invalid types
  -- PrimCD32, PrimCD64, PrimCD128. but we'll see.
  PrimVoid,
  PrimNullptr
  #-}

-- | Signed integer type tags
data SIntegerCategory
  = SICInt
  | SICChar
  | SICShort
  | SICLong
  | SICLongLong
  | SICBitInt
  deriving (Eq, Show)

-- | Integer types where 'Sign' is applicable. @char@ is excluded,
-- though @signed char@ and @unsigned char@ are both included.
data PrimSignedInteger
  = -- | The last field is only relevant for the case of 'SICBitInt'.
    -- If it's not an 'SICBitInt', then literally any number can go.
    PSI !SIntegerCategory !Sign !BitSize

-- | Construct a 'PrimSignedInteger' with bit width 0, and match
-- against a 'PrimSignedInteger' regardless of bit width.
pattern PSI0 :: SIntegerCategory -> Sign -> PrimSignedInteger
pattern PSI0 i s <- PSI i s ~_
  where
    PSI0 i s = PSI i s 0

-- | Bit width gets ignored except for @\_BitInt(...)@.
instance Eq PrimSignedInteger where
  PSI SICBitInt s w == PSI SICBitInt t v = s == t && w == v
  PSI c s ~_ == PSI d t ~_ = c == d && s == t

-- | Bit width information gets hidden except for @\_BitInt(...)@.
instance Show PrimSignedInteger where
  showsPrec d (PSI SICBitInt s w) =
    showParen (d > 10) $
      ("PSI SICBitInt " ++) . showsPrec 11 s . (" " ++) . showsPrec 11 w
  showsPrec d (PSI c s ~_) =
    showParen (d > 10) $
      ("PSI " ++) . showsPrec 11 c . (" " ++) . showsPrec 11 s

-- | For a signed or unsigned integer type, get the sign information.
-- For @\_BitInt(...)@ types, also get the bit int width.
pi_si :: Prism' Prim PrimSignedInteger
pi_si = prism' make destroy
 where
  make = \case
    PSI SICInt s ~_ -> PInt s
    PSI SICChar s ~_ -> PSUChar s
    PSI SICShort s ~_ -> PShort s
    PSI SICLong s ~_ -> PLong s
    PSI SICLongLong s ~_ -> PLongLong s
    PSI SICBitInt s w -> PBitInt s w
  destroy = \case
    PInt s -> Just $ PSI0 SICInt s
    PSUChar s -> Just $ PSI0 SICChar s
    PShort s -> Just $ PSI0 SICShort s
    PLong s -> Just $ PSI0 SICLong s
    PLongLong s -> Just $ PSI0 SICLongLong s
    PBitInt s w -> Just $ PSI SICBitInt s w
    ~_ -> Nothing
