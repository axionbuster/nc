-- | A fragment of the parser for C.
module Language.NC.Parse.Frag1 where

import Language.NC.Experiment.Types
import Language.NC.Internal.Prelude
import Language.NC.Lex2

-- i will parse PT and then merge it into a PrimType value
-- that's being unfolded.
data PT
  = CarrySign Signed
  | CarryInt PTIntType
  | CarryFloat RealFloatType
  | CarryComplex
  | CarryChar
  | CarryVoid

data PTIntType
  = PTChar
  | PTShort
  | PTInt
  | PTLong
  | PTLongLong
  | PTBitInt

-- record counts of each token occurrence
data PTSummary
  = PTSummary
  { pssigned :: Maybe Signed,
    psbitint :: Word8,
    psbitintwidth :: Word8, -- custom field, populated with bitint
    psint :: Word8,
    psshort :: Word8,
    pschar :: Word8,
    pslong :: Word8,
    psfloat :: Word8,
    psdouble :: Word8,
    pscomplex :: Word8,
    psdecimal :: Word8,
    psdecimalbits :: Word8, -- flexible membership as impl. changes
    psvoid :: Word8
  }
  deriving (Show)

psbasiccheck :: PTSummary -> Bool
psbasiccheck p =
  let b = p.psbitint
      i = p.psint
      s = p.psshort
      c = p.pschar
      l = p.pslong
      f = p.psfloat
      d = p.psdouble
      o = p.pscomplex
      m = p.psdecimal
      v = p.psvoid
   in (b <= 1)
        && (i <= 1)
        && (s <= 1)
        && (c <= 1)
        && (l <= 2) -- long and long long
        && (f <= 1)
        && (d <= 1)
        && (o <= 1)
        && (m <= 1)
        && (v <= 1)

psanyfloat :: PTSummary -> Bool
psanyfloat p =
  (psfloat p > 0)
    || (psdouble p > 0)
    || (pscomplex p > 0)
    || (psdecimal p > 0)
    || (psdecimalbits p > 0)

psanyint :: PTSummary -> Bool
psanyint p = q > 0 || i > 0 || s > 0 || c > 0 || l > 0
  where
    q = psbitint p
    i = psint p
    s = psshort p
    c = pschar p
    l = pslong p

pscoercesign :: PTSummary -> Signed
pscoercesign p
  | Just s <- pssigned p = s
  | otherwise = Signed

pscvt :: PTSummary -> Parser PrimType
pscvt p
  | not (psbasiccheck p) = throwbasic "pscvt: too many specifiers"
  | psvoid p > 0 =
      if psanyint p || psanyfloat p
        then throwbasic "pscvt: void and other types"
        else case p.pssigned of
          Just _ -> throwbasic "pscvt: (un)signed void"
          Nothing -> pure Void
  | pschar p > 0 =
      if (p.psbitint > 0)
        || (p.psint > 0)
        || (p.psshort > 0)
        || (p.pslong > 0)
        || psanyfloat p
        then throwbasic "pscvt: char and other types"
        else pure $ Char (pssigned p)
  | psbitint p > 0 =
      if (p.psint > 0)
        || (p.psshort > 0)
        || (p.pslong > 0)
        || psanyfloat p
        then throwbasic "pscvt: _BitInt(...) and other types"
        else
          if p.psbitintwidth == 0
            then throwbasic "pscvt: _BitInt(0)"
            else
              pure
                $ Int (pscoercesign p)
                $ BitInt
                $ fromIntegral p.psbitintwidth
  | psint p > 0 =
      if psanyfloat p || (p.psshort > 0 && p.pslong > 0)
        then throwbasic "pscvt: int and other types"
        else
          pure
            $ Int
              (pscoercesign p)
              if
                | p.psshort > 0 -> Short
                | p.pslong == 1 -> Long
                | p.pslong == 2 -> LongLong
                | otherwise -> IntLen
  | psshort p > 0 =
      if psanyfloat p || p.pslong > 0
        then throwbasic "pscvt: short and other types"
        else pure $ Int (pscoercesign p) Short
  | pslong p > 1 -- long long
    =
      if psanyfloat p
        then throwbasic "pscvt: long long and other types"
        else pure $ Int (pscoercesign p) LongLong
  | psfloat p > 0 =
      if (p.psdouble > 0)
        || (p.psdecimal > 0)
        || (p.psdecimalbits > 0)
        || (p.pslong > 0)
        then throwbasic "pscvt: float and other types"
        else
          if p.pssigned /= Nothing
            then throwbasic "pscvt: (un)signed float"
            else
              if p.pscomplex > 0
                then pure $ Float (Complex RFFloat)
                else pure $ Float (Real RFFloat)
  | psdouble p > 0 =
      if p.psfloat > 0 || p.psdecimal > 0 || p.psdecimalbits > 0
        then throwbasic "pscvt: double and other types"
        else
          if p.pssigned /= Nothing
            then throwbasic "pscvt: (un)signed double"
            else do
              subty <- case p.pslong of
                0 -> pure RFDouble
                1 -> pure RFLongDouble
                _ -> throwbasic "pscvt: long long double"
              if p.pscomplex > 0
                then pure $ Float (Complex subty)
                else pure $ Float (Real subty)
  | pslong p > 0 =
      if psanyfloat p
        then throwbasic "pscvt: long and other types"
        else pure $ Int (pscoercesign p) Long
  | psdecimal p > 0 =
      if p.pscomplex > 0
        then throwbasic "pscvt: decimal complex"
        else
          if p.pssigned /= Nothing
            then throwbasic "pscvt: (un)signed decimal"
            else case p.psdecimalbits of
              32 -> pure $ Float (Real RFDecimal32)
              64 -> pure $ Float (Real RFDecimal64)
              128 -> pure $ Float (Real RFDecimal128)
              _ -> throwbasic "pscvt: unsupported decimal bits"
  | otherwise =
      throwbasic
        $ "pscvt: unsupported or empty type summary: "
        ++ show p

constexpr = error "constexpr: not implemented"
