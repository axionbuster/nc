-- | Primitive non-derived unqualified C types.
module Language.NC.Internal.Types.PrimTypes (
  PrimType (..),
  pattern Bool_,
  pattern Void_,
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
  pattern BitInt_,
  pattern UBitInt_,
  pattern Decimal128_,
  pattern Decimal32_,
  pattern Decimal64_,
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
  Float,
  Int,
 )

-- | Non-derived, primitive types
data PrimType
  = -- | @_Bool@
    PTBool
  | -- | Non-character integral types, but not @char@ or @_Bool@
    PTInt Signed IntLen
  | -- | Character types
    PTChar (Maybe Signed)
  | -- | FTReal or complex floating point
    PTFloat FloatType
  | -- | The @void@ type
    PTVoid
  deriving (Eq, Show)

-- | If this is a @char@ then make it @signed char@. Otherwise, do nothing.
char2schar :: PrimType -> PrimType
char2schar (PTChar Nothing) = PTChar (Just Signed)
char2schar p = p

-- | A 'Setter' for the 'Signed' component of the 'PrimSign'
--
-- Caveat: If this is a non-signed @char@ then no change will occur.
_primsign :: Setter' PrimType Signed
_primsign = sets \chgsgn -> \case
  PTBool -> PTBool
  PTInt s ~i -> PTInt (chgsgn s) i
  PTChar Nothing -> PTChar Nothing
  PTChar (Just s) -> PTChar (Just (chgsgn s))
  PTFloat ~f -> PTFloat f
  PTVoid -> PTVoid

-- | This is a combination of '_primsign' and 'char2schar'.
_primsign2 :: Setter' PrimType Signed
_primsign2 = sets \chgsgn -> \case
  PTBool -> PTBool
  PTInt s ~i -> PTInt (chgsgn s) i
  PTChar Nothing -> PTChar (Just (chgsgn Signed))
  PTChar (Just s) -> PTChar (Just (chgsgn s))
  PTFloat ~f -> PTFloat f
  PTVoid -> PTVoid

-- | Some convenience patterns for 'PrimType'
pattern Bool_, Void_ :: PrimType
pattern Bool_ = PTBool
pattern Void_ = PTVoid

-- | Some convenience patterns for 'PrimType'
pattern Int_, Char_, SChar_, UChar_ :: PrimType
pattern Int_ = PTInt Signed ILInt
pattern Char_ = PTChar Nothing
pattern SChar_ = PTChar (Just Signed)
pattern UChar_ = PTChar (Just Unsigned)

-- | Some convenience patterns for 'PrimType'
pattern Float_, Double_, LongDouble_ :: PrimType
pattern Float_ = PTFloat (FTReal RFFloat)
pattern Double_ = PTFloat (FTReal RFDouble)
pattern LongDouble_ = PTFloat (FTReal RFLongDouble)

-- | Some convenience patterns for 'PrimType'
pattern ComplexFloat_, ComplexDouble_, ComplexLongDouble_ :: PrimType
pattern ComplexFloat_ = PTFloat (FTComplex RFFloat)
pattern ComplexDouble_ = PTFloat (FTComplex RFDouble)
pattern ComplexLongDouble_ = PTFloat (FTComplex RFLongDouble)

-- | Some convenience patterns for 'PrimType'
pattern Short_, Long_, LongLong_ :: PrimType
pattern Short_ = PTInt Signed ILShort
pattern Long_ = PTInt Signed ILLong
pattern LongLong_ = PTInt Signed ILLongLong

-- | Some convenience patterns for 'PrimType'
pattern BitInt_, UBitInt_ :: Word16 -> PrimType
pattern BitInt_ bw = PTInt Signed (ILBitInt bw)
pattern UBitInt_ bw = PTInt Unsigned (ILBitInt bw)

-- | Some convenience patterns for 'PrimType'
pattern UInt_, UShort_, ULong_, ULongLong_ :: PrimType
pattern UInt_ = PTInt Unsigned ILInt
pattern UShort_ = PTInt Unsigned ILShort
pattern ULong_ = PTInt Unsigned ILLong
pattern ULongLong_ = PTInt Unsigned ILLongLong

-- | Some convenience patterns for 'PrimType'
pattern Decimal32_, Decimal64_, Decimal128_ :: PrimType
pattern Decimal32_ = PTFloat (FTReal RFDecimal32)
pattern Decimal64_ = PTFloat (FTReal RFDecimal64)
pattern Decimal128_ = PTFloat (FTReal RFDecimal128)

-- | Signed?
data Signed = Signed | Unsigned
  deriving (Eq, Show)

-- | @int@, @short@, @long@, or @long long@
data IntLen
  = ILShort
  | ILInt
  | ILLong
  | ILLongLong
  | ILBitInt Word16
  deriving (Eq, Show)

-- | Real or complex and what type?
data FloatType = FTReal RealFloatType | FTComplex RealFloatType
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
