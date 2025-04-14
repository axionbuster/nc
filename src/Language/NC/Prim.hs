-- |
-- Module      : Language.NC.Prim
-- Description : Primitive C language types and their efficient encoding
-- Copyright   : (c) 2024-2025
-- License     : BSD-3-Clause
-- Maintainer  : axionbuster@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module defines the representation of primitive C types in the NC
-- compiler. Types are encoded in a space-efficient bit-packed representation as
-- a 'Word16':
--
-- * Primitive types include standard C types like int, char, long, float, etc.
-- * The bit layout encodes information about signedness, size, and special
--   categories
-- * Special provision is made for C23's _BitInt(N) type with variable bit width
--
-- The implementation uses a binary encoding that combines type category bits
-- with additional metadata like signedness (bit 15) and whether it's a _BitInt
-- (bit 14).
--
-- This module also includes support for parsing @\_Complex \_Decimal...@ types,
-- which do not exist in standard C. However, it allows expressing the types
-- which can be used for error reporting and perhaps more.
module Language.NC.Prim (
  -- * Types
  Prim,
  Sign (..),
  Complex (..),
  FloatCategory (..),
  PrimInfo (..),
  Word16,

  -- * Lenses
  pr_info,
  pc_si,

  -- * Other
  bitintwidth_ok,

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
) where

import Control.Lens
import Data.Bits
import Data.Word
import Prelude

-- Bit layout of Word16 representation:
--   Bit 15 (0x8000): Sign bit (1 = Signed, 0 = Unsigned)
--   Bit 14 (0x4000): _BitInt flag (1 = BitInt type)
--   For non-_BitInt types, the encoding works as follows:
--     Bits 5-6: Basic type category
--       00: int or short (bit 7 differentiates: 0 = int, 1 = short)
--       01: char types (bit 7 = 0 for NSChar or 1 for SUChar)
--       10: long or long long (bit 7 differentiates: 0 = long, 1 = long long)
--       11: _Bool (0x00C0)
--     Special types:
--       0x00E0: void
--       0x00F0: nullptr_t
--     Floating point types:
--       0x0008: Real double           0x000C: Complex double
--       0x0010: Real float            0x0014: Complex float
--       0x0018: Real long double      0x001C: Complex long double
--       0x0030: Real decimal32
--       0x0031: Real decimal64
--       0x0032: Real decimal128
--     For floating point types:
--       Bits 0-4: Encode float category
--       Bit 2 (0x0004): Complex flag (1 = Complex, 0 = Real)

-- | Primitive, non-derived types.
newtype Prim = Prim Word16
  deriving (Eq) via Word16

instance Show Prim where
  showsPrec d s =
    showParen (d > 10) $
      ("Prim " ++) . showsPrec 11 (s ^. pr_info)

-- | Go back and forth between the real 'Prim' representation
-- and the information stored.
pr_info :: Lens' Prim PrimInfo
pr_info = lens getter setter
 where
  getter (Prim w)
    | w .&. 0x4000 /= 0 =
        -- If the '_BitInt' bit is set, then the low 14 bits are used
        -- as the bit int width. Now that that's been read,
        -- we just need to determine the sign by inspecting the sign bit.
        PrimBitInt
          (if w .&. 0x8000 /= 0 then Signed else Unsigned)
          (w .&. 0x3FFF)
    | otherwise = case w of -- Look at bits 5-6-7 first
        0x0000 -> PrimInt Unsigned -- bits 5-6-7=000: unsigned int
        0x0080 -> PrimShort Unsigned -- bits 5-6-7=100: unsigned short
        0x0020 -> PrimNSChar -- bits 5-6-7=001: NSChar (char)
        0x00A0 -> PrimSUChar Unsigned -- bits 5-6-7=101: unsigned char
        0x0040 -> PrimLong Unsigned -- bits 5-6-7=010: unsigned long
        0x00C0 -> PrimLongLong Unsigned -- bits 5-6-7=110: unsigned long long
        0x0060 -> PrimBool -- bits 5-6-7=011: _Bool
        0x00E0 -> PrimVoid -- bits 5-6-7=111: void
         -- Handle special cases and signed variants
        0x00F0 -> PrimNullptr -- nullptr_t
        0x8000 -> PrimInt Signed -- signed int
        0x8080 -> PrimShort Signed -- signed short
        0x80A0 -> PrimSUChar Signed -- signed char
        0x8040 -> PrimLong Signed -- signed long
        0x80C0 -> PrimLongLong Signed -- signed long long
        -- floating points
        0x0008 -> PrimFloat CxReal FCDouble
        0x0010 -> PrimFloat CxReal FCFloat
        0x0018 -> PrimFloat CxReal FCLongDouble
        0x0030 -> PrimFloat CxReal FCDecimal32
        0x0031 -> PrimFloat CxReal FCDecimal64
        0x0032 -> PrimFloat CxReal FCDecimal128
        0x000C -> PrimFloat CxComplex FCDouble
        0x0014 -> PrimFloat CxComplex FCFloat
        0x001C -> PrimFloat CxComplex FCLongDouble
        0x0034 -> PrimFloat CxComplex FCDecimal32
        0x0035 -> PrimFloat CxComplex FCDecimal64
        0x0036 -> PrimFloat CxComplex FCDecimal128
        _ -> error $ "Unknown bit pattern in Prim: " ++ show w
  setter (Prim _) =
    Prim . \case
      i
        | Just (PSI SICBitInt sign bw) <- i ^? pc_si ->
            -- limited validation
            if bw .&. 0xC000 /= 0
              then
                error $
                  "bit width to encode ("
                    ++ show bw
                    ++ ") exceeds 14 bits"
              else bw .|. 0x4000 .|. case sign of Signed -> 0x8000; _ -> 0
        | Just (PSI category sign _) <- i ^? pc_si ->
            let cb = case category of
                  SICInt -> 0x0000 -- bits 5-6-7=000
                  SICShort -> 0x0080 -- bits 5-6-7=100
                  SICChar -> 0x00A0 -- bits 5-6-7=101
                  SICLong -> 0x0040 -- bits 5-6-7=010
                  SICLongLong -> 0x00C0 -- bits 5-6-7=110
                sb = case sign of Signed -> 0x8000; _ -> 0
             in cb .|. sb
      PrimBool -> 0x0060 -- bits 5-6-7=011
      PrimNSChar -> 0x0020 -- bits 5-6-7=001
      PrimVoid -> 0x00E0 -- bits 5-6-7=111
      PrimNullptr -> 0x00F0 -- special case
      PrimFloat complex category ->
        let cb = case complex of CxReal -> 0; _ -> 0x0004
            fb = case category of
              FCFloat -> 0x0010
              FCDouble -> 0x0008
              FCLongDouble -> 0x0018
              FCDecimal32 -> 0x0030
              FCDecimal64 -> 0x0031
              FCDecimal128 -> 0x0032
         in cb .|. fb
      _ -> error "impossible"

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

-- | Normally you would derive a 'PrimInfo' from a 'Prim' first
-- to extract useful information.
data PrimInfo
  = -- | @int@
    PrimInt !Sign
  | -- | @\_Bool@
    PrimBool
  | -- | @signed char@ or @unsigned char@, but not regular @char@.
    PrimSUChar !Sign
  | -- | no-sign @char@ (regular @char@)
    PrimNSChar
  | -- | @short@
    PrimShort !Sign
  | -- | @long@
    PrimLong !Sign
  | -- | @long long@
    PrimLongLong !Sign
  | -- | @\_BitInt(N)@
    PrimBitInt !Sign !Word16
  | -- | floating point types
    PrimFloat !Complex !FloatCategory
  | -- | @void@, sort of corresponds to 'Data.Void.Void'
    PrimVoid
  | -- | @nullptr\_t@, sort of corresponds to @()@
    PrimNullptr
  deriving (Eq, Show)

-- | Signed integer type tags
data SIntegerCategory
  = SICInt
  | SICChar
  | SICShort
  | SICLong
  | SICLongLong
  | SICBitInt
  deriving (Eq, Show)

-- | Integer types where 'Sign' is applicable. 'Char' is excluded.
data PrimSignedInteger
  = -- | The last field is only relevant for the case of 'SICBitInt'.
    -- If it's not an 'SICBitInt', then literally any number can go.
    PSI !SIntegerCategory !Sign !Word16

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
pc_si :: Prism' PrimInfo PrimSignedInteger
pc_si = prism' make destroy
 where
  make = \case
    PSI SICInt s ~_ -> PrimInt s
    PSI SICChar s ~_ -> PrimSUChar s
    PSI SICShort s ~_ -> PrimShort s
    PSI SICLong s ~_ -> PrimLong s
    PSI SICLongLong s ~_ -> PrimLongLong s
    PSI SICBitInt s w -> PrimBitInt s w
  destroy = \case
    PrimInt s -> Just $ PSI0 SICInt s
    PrimSUChar s -> Just $ PSI0 SICChar s
    PrimShort s -> Just $ PSI0 SICShort s
    PrimLong s -> Just $ PSI0 SICLong s
    PrimLongLong s -> Just $ PSI0 SICLongLong s
    PrimBitInt s w -> Just $ PSI SICBitInt s w
    ~_ -> Nothing

-- | If the bitwidth is OK, wrap it inside 'Just'.
--
-- NOTE: for signed @\_BitInt@, bit width must be at least 2. For unsigned,
-- it must be at least 1. The signed @\_BitInt@ case isn't checked.
-- It only checks whether it is positive and fits in 14 bits.
bitintwidth_ok :: Word16 -> Maybe Word16
bitintwidth_ok a
  | a > 0x3FFF || a == 0 = Nothing
  | otherwise = Just a

-- * Common primitive type constants for easier use

-- | Signed @int@ type in C
pr_int :: Prim
pr_int = Prim 0x8000

-- | Unsigned @int@ type in C
pr_uint :: Prim
pr_uint = Prim 0x0000

-- | Signed @long@ type in C
pr_long :: Prim
pr_long = Prim 0x8040

-- | Unsigned @long@ type in C
pr_ulong :: Prim
pr_ulong = Prim 0x0040

-- | Signed @short@ type in C
pr_short :: Prim
pr_short = Prim 0x8080

-- | Unsigned @short@ type in C
pr_ushort :: Prim
pr_ushort = Prim 0x0080

-- | Regular @char@ type in C (no explicit sign)
pr_char :: Prim
pr_char = Prim 0x0020

-- | Explicit @signed char@ type in C
pr_schar :: Prim
pr_schar = Prim 0x80A0

-- | Explicit @unsigned char@ type in C
pr_uchar :: Prim
pr_uchar = Prim 0x00A0

-- | Signed @long long@ type in C
pr_longlong :: Prim
pr_longlong = Prim 0x80C0

-- | Unsigned @long long@ type in C
pr_ulonglong :: Prim
pr_ulonglong = Prim 0x00C0

-- | Boolean type (@_Bool@) in C
pr_bool :: Prim
pr_bool = Prim 0x0060

-- | The @void@ type in C
pr_void :: Prim
pr_void = Prim 0x00E0

-- | The @nullptr_t@ type in C++
pr_nullptr :: Prim
pr_nullptr = Prim 0x00F0

-- | Real @float@ type in C
pr_float :: Prim
pr_float = Prim 0x0010

-- | Real @double@ type in C
pr_double :: Prim
pr_double = Prim 0x0008

-- | Real @long double@ type in C
pr_longdouble :: Prim
pr_longdouble = Prim 0x0018

-- | Complex @float@ type in C
pr_cfloat :: Prim
pr_cfloat = Prim 0x0014

-- | Complex @double@ type in C
pr_cdouble :: Prim
pr_cdouble = Prim 0x000C

-- | Complex @long double@ type in C
pr_clongdouble :: Prim
pr_clongdouble = Prim 0x001C

-- | IEEE 754-2008 @_Decimal32@ type
pr_decimal32 :: Prim
pr_decimal32 = Prim 0x0030

-- | IEEE 754-2008 @_Decimal64@ type
pr_decimal64 :: Prim
pr_decimal64 = Prim 0x0031

-- | IEEE 754-2008 @_Decimal128@ type
pr_decimal128 :: Prim
pr_decimal128 = Prim 0x0032

-- | Complex IEEE 754-2008 @_Decimal32@ type
pr_cdecimal32 :: Prim
pr_cdecimal32 = Prim 0x0034

-- | Complex IEEE 754-2008 @_Decimal64@ type
pr_cdecimal64 :: Prim
pr_cdecimal64 = Prim 0x0035

-- | Complex IEEE 754-2008 @_Decimal128@ type
pr_cdecimal128 :: Prim
pr_cdecimal128 = Prim 0x0036
