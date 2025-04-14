{-# LANGUAGE StrictData #-}

-- | Primitive non-derived unqualified C types.
module Language.NC.Internal.Types.PrimTypes (
  PrimType (..),
  PrimTypeCategory (..),
  IntType (..),
  Signedness (..),
  RealFloatType (..),
  FloatDimension (..),
  BitIntWidth,
  _pt_cat,
  pt_cat,
  pt_getsign,
  pt_setsign,
  pt_getinttype,
  pt_setinttype,
  pt_getbitwidth,
  pt_setbitwidth,
  pt_getrealfloattype,
  pt_setrealfloattype,
) where

import Control.Lens
import Data.Word
import Prelude hiding (
  Bool,
  Char,
  Double,
  Float,
  Int,
 )

-- | Type type to represent the bit widths of integral types.
-- I didn't know this at first, but the type appears to be changing often
-- as of writing.
type BitIntWidth = Word16

-- | Non-derived, primitive types
data PrimType
  = -- | @\_Bool@
    PTBool
  | -- | @void@
    PTVoid
  | -- | @signed int@
    PTInt
  | -- | @unsigned int@
    PTUInt
  | -- | @signed short@
    PTShort
  | -- | @unsigned short@
    PTUShort
  | -- | @signed long@
    PTLong
  | -- | @unsigned long@
    PTULong
  | -- | @signed long long@
    PTLongLong
  | -- | @unsigned long long@
    PTULongLong
  | -- | plain @char@
    PTChar
  | -- | @signed char@
    PTSChar
  | -- | @unsigned char@
    PTUChar
  | -- | @float@
    PTFloat
  | -- | @double@
    PTDouble
  | -- | @long double@
    PTLongDouble
  | -- | @_Complex float@
    PTComplexFloat
  | -- | @_Complex double@
    PTComplexDouble
  | -- | @_Complex long double@
    PTComplexLongDouble
  | -- | @\_BitInt(N)@
    PTBitInt BitIntWidth
  | -- | @unsigned \_BitInt(N)@
    PTUBitInt BitIntWidth
  | -- | @\_Decimal32@
    PTDecimal32
  | -- | @\_Decimal64@
    PTDecimal64
  | -- | @\_Decimal128@
    PTDecimal128
  deriving (Eq, Show)

-- | 'PrimType' category: integral, floating, or @void@.
data PrimTypeCategory
  = -- | @\_Bool@, @char@, etc.
    PTCIntegral
  | -- | @float _Complex@, @\_Decimal32@, etc.
    PTCFloating
  | -- | @void@
    PTCVoid

-- | Integer type.
data IntType
  = ITBool
  | ITInt
  | ITShort
  | ITLong
  | ITLongLong
  | ITChar
  | ITBitInt
  deriving (Eq, Show)

-- | Signedness for an integer type:
--
--  - All integer types except @char@ are either 'Signed' or 'Unsigned'.
--  - For non-@char@-types, the 'NoSign' variant is the 'Signed' one.
--  - For @char@, any of the three variants is possible.
--  - Non-integral types do not have the concept of a sign. Setting their
--    sign is a no-op. Getting their sign will either succeed or fail.
data Signedness
  = Signed
  | Unsigned
  | NoSign

-- | A floating-point type except @\_Decimal\*@ is either real or complex
-- so is it @float@, @double@, @\_Decimal64@, etc.?
data RealFloatType
  = RFFloat
  | RFDouble
  | RFLongDouble
  | RFDecimal32
  | RFDecimal64
  | RFDecimal128

-- | Is the floating-point type real or complex? (We do not have a dedicated
-- imaginary type.)
data FloatDimension
  = FDReal
  | FDComplex

-- | Categorize a 'PrimType'
pt_cat :: Getter PrimType PrimTypeCategory
pt_cat = to _pt_cat

-- | Categorize a 'PrimType'
_pt_cat :: PrimType -> PrimTypeCategory
_pt_cat = \case
  PTBool
  PTInt
  PTUInt
  PTShort
  PTUShort
  PTLong
  PTULong
  PTLongLong
  PTULongLong
  PTChar
  PTSChar
  PTUChar
  PTBitInt{}
  PTUBitInt{} -> PTCIntegral
  PTVoid -> PTCVoid
  _ -> PTCFloating

-- | Get the integer type.
pt_getinttype :: Getter PrimType (Maybe IntType)
pt_getinttype = to \case
  PTBool -> Just ITBool
  PTInt; PTUInt -> Just ITInt
  PTShort; PTUShort -> Just ITShort
  PTLong; PTULong -> Just ITLong
  PTLongLong; PTULongLong -> Just ITLongLong
  PTChar; PTUChar; PTSChar -> Just ITChar
  PTBitInt{}; PTUBitInt{} -> Just ITBitInt
  _ -> Nothing

_map_inttype :: IntType -> PrimType -> PrimType
_map_inttype it p = case p ^. pt_getsign of
  Just Signed -> case it of
    ITInt -> PTInt
    ITShort -> PTShort
    ITLong -> PTShort
    ITLongLong -> PTLongLong
    ITChar -> PTSChar
    ITBitInt -> case p ^. pt_getbitwidth of
      Just bw -> PTBitInt bw
      _ -> p
    ITBool -> PTBool
  Just Unsigned -> case it of
    ITInt -> PTUInt
    ITShort -> PTUShort
    ITLong -> PTULong
    ITLongLong -> PTULongLong
    ITChar -> PTUChar
    ITBitInt -> case p ^. pt_getbitwidth of
      Just bw -> PTUBitInt bw
      _ -> p
    ITBool -> PTBool
  Just NoSign -> case it of
    ITInt -> PTInt
    ITShort -> PTShort
    ITLong -> PTShort
    ITLongLong -> PTLongLong
    ITChar -> PTChar
    ITBitInt -> case p ^. pt_getbitwidth of
      Just bw -> PTBitInt bw
      _ -> p
    ITBool -> PTBool
  Nothing -> p

-- | Set the integer type
pt_setinttype :: Setter' PrimType IntType
pt_setinttype = sets \f p ->
  case p ^. pt_getinttype of
    Just t -> _map_inttype (f t) p
    _ -> p

_map_signedness :: Signedness -> PrimType -> PrimType
_map_signedness Signed = \case
  PTInt; PTUInt -> PTInt
  PTShort; PTUShort -> PTShort
  PTLong; PTULong -> PTLong
  PTLongLong; PTULongLong -> PTLongLong
  PTChar; PTSChar; PTUChar -> PTSChar
  PTUBitInt b -> PTBitInt b
  t -> t
_map_signedness Unsigned = \case
  PTInt; PTUInt -> PTUInt
  PTShort; PTUShort -> PTUShort
  PTLong; PTULong -> PTULong
  PTLongLong; PTULongLong -> PTULongLong
  PTChar; PTSChar; PTUChar -> PTUChar
  PTBitInt b -> PTUBitInt b
  t -> t
_map_signedness NoSign = \case
  PTInt; PTUInt -> PTInt
  PTShort; PTUShort -> PTShort
  PTLong; PTULong -> PTLong
  PTLongLong; PTULongLong -> PTLongLong
  PTChar; PTSChar; PTUChar -> PTChar
  PTUBitInt b -> PTBitInt b
  t -> t

_get_signedness :: PrimType -> Maybe Signedness
_get_signedness = \case
  PTInt; PTShort; PTLong; PTLongLong; PTSChar; PTBitInt{} -> Just Signed
  PTUInt; PTUShort; PTULong; PTULongLong; PTUChar; PTUBitInt{} -> Just Unsigned
  PTChar -> Just NoSign
  _ -> Nothing

-- | Get the sign of a 'PrimType' if it's an integral type, or else return
-- 'Nothing'.
pt_getsign :: Getter PrimType (Maybe Signedness)
pt_getsign = to _get_signedness

-- | Set the sign of a 'PrimType'; for a non-integral type, this is a safe
-- no-op.
pt_setsign :: Setter' PrimType Signedness
pt_setsign = sets \chgsgn pt ->
  case pt ^. pt_getsign of
    Just sgn -> _map_signedness (chgsgn sgn) pt
    _ -> pt

-- | If possible, get the bit width of a @\_BitInt@ type.
pt_getbitwidth :: Getter PrimType (Maybe BitIntWidth)
pt_getbitwidth = to \case
  PTBitInt b -> Just b
  PTUBitInt b -> Just b
  _ -> Nothing

-- | Set the bit width of a @\_BitInt@ type, ignoring if it's not that type.
pt_setbitwidth :: Setter' PrimType BitIntWidth
pt_setbitwidth = sets \chgbw pt ->
  case pt of
    PTBitInt b -> PTBitInt (chgbw b)
    PTUBitInt b -> PTUBitInt (chgbw b)
    t -> t

-- | Get the real float type.
pt_getrealfloattype :: Getter PrimType (Maybe RealFloatType)
pt_getrealfloattype = to \case
  PTFloat; PTComplexFloat -> Just RFFloat
  PTDouble; PTComplexDouble -> Just RFDouble
  PTLongDouble; PTComplexLongDouble -> Just RFLongDouble
  PTDecimal128 -> Just RFDecimal128
  PTDecimal32 -> Just RFDecimal32
  PTDecimal64 -> Just RFDecimal64
  _ -> Nothing

_pt_fldim :: PrimType -> FloatDimension
_pt_fldim = \case
  PTFloat; PTDouble; PTLongDouble; PTDecimal128; PTDecimal32; PTDecimal64 ->
    FDReal
  _ -> FDComplex

_assemble_float :: RealFloatType -> FloatDimension -> Maybe PrimType
_assemble_float = curry \case
  (RFFloat, FDReal) -> Just PTFloat
  (RFDouble, FDReal) -> Just PTDouble
  (RFLongDouble, FDReal) -> Just PTLongDouble
  (RFDecimal128, FDReal) -> Just PTDecimal128
  (RFDecimal32, FDReal) -> Just PTDecimal32
  (RFDecimal64, FDReal) -> Just PTDecimal64
  (RFFloat, FDComplex) -> Just PTComplexFloat
  (RFDouble, FDComplex) -> Just PTComplexDouble
  (RFLongDouble, FDComplex) -> Just PTComplexLongDouble
  _ -> Nothing

-- | Set the real floating point type.
pt_setrealfloattype :: Setter' PrimType RealFloatType
pt_setrealfloattype = sets \f p ->
  case p ^. pt_getrealfloattype of
    Just ft -> maybe p id (_assemble_float (f ft) (_pt_fldim p))
    _ -> p
