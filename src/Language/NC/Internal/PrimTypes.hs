-- | Primitive non-derived pretypes.
module Language.NC.Internal.PrimTypes (
  PrimType (..),
  pattern Int_,
  pattern Char_,
  pattern SChar_,
  pattern UChar_,
  pattern Float_,
  pattern Double_,
  pattern LongDouble_,
  pattern ComplexFloat_,
  pattern ComplexDouble_,
  pattern ComplexLongDouble_,
  pattern Short_,
  pattern Long_,
  pattern LongLong_,
  pattern UInt_,
  pattern UShort_,
  pattern ULong_,
  pattern ULongLong_,
  Signed (..),
  IntLen (..),
  FloatType (..),
  RealFloatType (..),
  char2schar,
  _primsign,
  _primsign2,
) where

import Control.Lens.Setter
import Data.Word
import Prelude hiding (
  Bool,
  Char,
  Double,
  Enum,
  Float,
  Int,
 )

-- | Non-derived, primitive types
data PrimType
  = -- | @_Bool@
    Bool
  | -- | Non-character integral types, but not @char@ or @_Bool@
    Int Signed IntLen
  | -- | Character types
    Char (Maybe Signed)
  | -- | Real or complex floating point
    Float FloatType
  | -- | The @void@ type
    Void
  deriving (Eq, Show)

-- | If this is a @char@ then make it @signed char@. Otherwise, do nothing.
char2schar :: PrimType -> PrimType
char2schar (Char Nothing) = Char (Just Signed)
char2schar p = p

-- | A 'Setter' for the 'Signed' component of the 'PrimSign'
--
-- Caveat: If this is a non-signed @char@ then no change will occur.
_primsign :: Setter' PrimType Signed
_primsign = sets \chgsgn -> \case
  Bool -> Bool
  Int s ~i -> Int (chgsgn s) i
  Char Nothing -> Char Nothing
  Char (Just s) -> Char (Just (chgsgn s))
  Float ~f -> Float f
  Void -> Void

-- | This is a combination of '_primsign' and 'char2schar'.
_primsign2 :: Setter' PrimType Signed
_primsign2 = sets \chgsgn -> \case
  Bool -> Bool
  Int s ~i -> Int (chgsgn s) i
  Char Nothing -> Char (Just (chgsgn Signed))
  Char (Just s) -> Char (Just (chgsgn s))
  Float ~f -> Float f
  Void -> Void

-- | Some convenience patterns for 'PrimType'
pattern Int_, Char_, SChar_, UChar_ :: PrimType
pattern Int_ = Int Signed IntLen
pattern Char_ = Char Nothing
pattern SChar_ = Char (Just Signed)
pattern UChar_ = Char (Just Unsigned)

-- | Some convenience patterns for 'PrimType'
pattern Float_, Double_, LongDouble_ :: PrimType
pattern Float_ = Float (Real RFFloat)
pattern Double_ = Float (Real RFDouble)
pattern LongDouble_ = Float (Real RFLongDouble)

-- | Some convenience patterns for 'PrimType'
pattern ComplexFloat_, ComplexDouble_, ComplexLongDouble_ :: PrimType
pattern ComplexFloat_ = Float (Complex RFFloat)
pattern ComplexDouble_ = Float (Complex RFDouble)
pattern ComplexLongDouble_ = Float (Complex RFLongDouble)

-- | Some convenience patterns for 'PrimType'
pattern Short_, Long_, LongLong_ :: PrimType
pattern Short_ = Int Signed Short
pattern Long_ = Int Signed Long
pattern LongLong_ = Int Signed LongLong

-- | Some convenience patterns for 'PrimType'
pattern UInt_, UShort_, ULong_, ULongLong_ :: PrimType
pattern UInt_ = Int Unsigned IntLen
pattern UShort_ = Int Unsigned Short
pattern ULong_ = Int Unsigned Long
pattern ULongLong_ = Int Unsigned LongLong

-- | Signed?
data Signed = Signed | Unsigned
  deriving (Eq, Show)

-- | @int@, @short@, @long@, or @long long@
data IntLen
  = Short
  | IntLen
  | Long
  | LongLong
  | BitInt Word8
  deriving (Eq, Show, Ord)

-- | Real or complex and what type?
data FloatType = Real RealFloatType | Complex RealFloatType
  deriving (Eq, Show)

-- | @float@, @double@, or @long double@?
data RealFloatType
  = RFFloat
  | RFDouble
  | RFLongDouble
  | RFDecimal128
  | RFDecimal32
  | RFDecimal64
  deriving (Eq, Show)
