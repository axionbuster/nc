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
  Prim,
  BitSize,
  -- Pattern synonyms that replace PrimInfo
  pattern PrimInt,
  pattern PrimBool,
  pattern PrimSUChar,
  pattern PrimNSChar,
  pattern PrimShort,
  pattern PrimLong,
  pattern PrimLongLong,
  pattern PrimBitInt,
  pattern PrimFloat,
  pattern PrimVoid,
  pattern PrimNullptr,
  PrimSignedInteger (..),
  SIntegerCategory (..),
  Sign (..),
  Complex (..),
  FloatCategory (..),

  -- * Lenses
  pi_si,

  -- * Common primitive type constants
  pr_int,
  pr_uint,
  pr_long,
  pr_ulong,
  pr_short,
  pr_ushort,
  pr_char,
  pr_uchar,
  pr_schar,
  pr_longlong,
  pr_ulonglong,
  pr_bool,
  pr_void,
  pr_nullptr,
  pr_float,
  pr_double,
  pr_longdouble,
  pr_cfloat,
  pr_cdouble,
  pr_clongdouble,
  pr_decimal32,
  pr_decimal64,
  pr_decimal128,
  pr_cdecimal32,
  pr_cdecimal64,
  pr_cdecimal128,
  pr_bitint,
  pr_ubitint,
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

-- | Pattern synonyms that replace the PrimInfo constructors

-- | C @int@
pattern PrimInt :: Sign -> Prim
pattern PrimInt s = PInt s

-- | C @\_Bool@
pattern PrimBool :: Prim
pattern PrimBool = PBool

-- | @signed char@ or @unsigned char@, but not regular @char@.
pattern PrimSUChar :: Sign -> Prim
pattern PrimSUChar s = PSUChar s

-- | no-sign @char@ (regular @char@)
pattern PrimNSChar :: Prim
pattern PrimNSChar = PNSChar

-- | C @short@
pattern PrimShort :: Sign -> Prim
pattern PrimShort s = PShort s

-- | C @long@
pattern PrimLong :: Sign -> Prim
pattern PrimLong s = PLong s

-- | C @long long@
pattern PrimLongLong :: Sign -> Prim
pattern PrimLongLong s = PLongLong s

-- | C @\_BitInt(N)@
pattern PrimBitInt :: Sign -> BitSize -> Prim
pattern PrimBitInt s w = PBitInt s w

-- | floating point types
pattern PrimFloat :: Complex -> FloatCategory -> Prim
pattern PrimFloat c fc = PFloat c fc

-- | @void@, sort of corresponds to 'Data.Void.Void'
pattern PrimVoid :: Prim
pattern PrimVoid = PVoid

-- | @nullptr\_t@, sort of corresponds to @()@
pattern PrimNullptr :: Prim
pattern PrimNullptr = PNullptr

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

-- * Common primitive type constants for easier use

-- | Signed @int@ type in C
pr_int :: Prim
pr_int = PInt Signed

-- | Unsigned @int@ type in C
pr_uint :: Prim
pr_uint = PInt Unsigned

-- | Signed @long@ type in C
pr_long :: Prim
pr_long = PLong Signed

-- | Unsigned @long@ type in C
pr_ulong :: Prim
pr_ulong = PLong Unsigned

-- | Signed @short@ type in C
pr_short :: Prim
pr_short = PShort Signed

-- | Unsigned @short@ type in C
pr_ushort :: Prim
pr_ushort = PShort Unsigned

-- | Regular @char@ type in C (no explicit sign)
pr_char :: Prim
pr_char = PNSChar

-- | Explicit @signed char@ type in C
pr_schar :: Prim
pr_schar = PSUChar Signed

-- | Explicit @unsigned char@ type in C
pr_uchar :: Prim
pr_uchar = PSUChar Unsigned

-- | Signed @long long@ type in C
pr_longlong :: Prim
pr_longlong = PLongLong Signed

-- | Unsigned @long long@ type in C
pr_ulonglong :: Prim
pr_ulonglong = PLongLong Unsigned

-- | Boolean type (@_Bool@) in C
pr_bool :: Prim
pr_bool = PBool

-- | The @void@ type in C
pr_void :: Prim
pr_void = PVoid

-- | The @nullptr_t@ type in C++
pr_nullptr :: Prim
pr_nullptr = PNullptr

-- | Real @float@ type in C
pr_float :: Prim
pr_float = PFloat CxReal FCFloat

-- | Real @double@ type in C
pr_double :: Prim
pr_double = PFloat CxReal FCDouble

-- | Real @long double@ type in C
pr_longdouble :: Prim
pr_longdouble = PFloat CxReal FCLongDouble

-- | Complex @float@ type in C
pr_cfloat :: Prim
pr_cfloat = PFloat CxComplex FCFloat

-- | Complex @double@ type in C
pr_cdouble :: Prim
pr_cdouble = PFloat CxComplex FCDouble

-- | Complex @long double@ type in C
pr_clongdouble :: Prim
pr_clongdouble = PFloat CxComplex FCLongDouble

-- | IEEE 754-2008 @_Decimal32@ type
pr_decimal32 :: Prim
pr_decimal32 = PFloat CxReal FCDecimal32

-- | IEEE 754-2008 @_Decimal64@ type
pr_decimal64 :: Prim
pr_decimal64 = PFloat CxReal FCDecimal64

-- | IEEE 754-2008 @_Decimal128@ type
pr_decimal128 :: Prim
pr_decimal128 = PFloat CxReal FCDecimal128

-- | Complex IEEE 754-2008 @_Decimal32@ type
pr_cdecimal32 :: Prim
pr_cdecimal32 = PFloat CxComplex FCDecimal32

-- | Complex IEEE 754-2008 @_Decimal64@ type
pr_cdecimal64 :: Prim
pr_cdecimal64 = PFloat CxComplex FCDecimal64

-- | Complex IEEE 754-2008 @_Decimal128@ type
pr_cdecimal128 :: Prim
pr_cdecimal128 = PFloat CxComplex FCDecimal128

-- | @\_BitInt(...)@ type
pr_bitint, pr_ubitint :: BitSize -> Prim
pr_bitint w = PBitInt Signed w
pr_ubitint w = PBitInt Unsigned w
