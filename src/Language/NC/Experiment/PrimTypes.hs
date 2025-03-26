-- | Experimental refactoring of non-derived primitive (pre)types.
module Language.NC.Experiment.PrimTypes
  ( PrimType (..),
    pattern Int_,
    pattern Char_,
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
  )
  where

import Language.NC.Internal.Prelude hiding
  ( Bool,
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

-- | Some convenience patterns for 'PrimType'
pattern Int_, Char_ :: PrimType
pattern Int_ = Int Signed IntLen
pattern Char_ = Char Nothing

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
data IntLen = IntLen | Short | Long | LongLong
  deriving (Eq, Show)

-- | Real or complex and what type?
data FloatType = Real RealFloatType | Complex RealFloatType
  deriving (Eq, Show)

-- | @float@, @double@, or @long double@?
data RealFloatType = RFFloat | RFDouble | RFLongDouble
  deriving (Eq, Show)
