-- | A fragment of the parser for C.
module Language.NC.Parse.Frag1 where

import Language.NC.Experiment.Types
import Language.NC.Internal.Prelude
import Language.NC.Lex2

-- read in a storage class specifier or fail.
-- when 'typedef' is encountered, it is treated as a NoSpecifier.
storclass =
  choice
    [ Auto <$ lexeme auto',
      Register <$ lexeme register',
      Extern <$ lexeme extern',
      Static <$ lexeme static',
      ThreadLocal <$ lexeme threadLocal',
      Constexpr <$ lexeme constexpr',
      Volatile <$ lexeme volatile',
      NoSpecifier <$ lexeme typedef' -- a bit of a hack
    ]

-- i just can't resist using Template Haskell

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
    psbitint :: Int8,
    psint :: Int8,
    psshort :: Int8,
    pschar :: Int8,
    pslong :: Int8,
    psfloat :: Int8,
    psdouble :: Int8,
    pscomplex :: Int8,
    psdecimal :: Int8,
    psdecimalbits :: Word8, -- 0, 32, 64, 128 (different from other fields)
    psvoid :: Int8
  }
  deriving (Show)

psbasiccheck :: PTSummary -> Bool
psbasiccheck p =
  let inr x y z = x >= y && x <= z
      b = p.psbitint
      i = p.psint
      s = p.psshort
      c = p.pschar
      l = p.pslong
      f = p.psfloat
      d = p.psdouble
      o = p.pscomplex
      m = p.psdecimal
      e = p.psdecimalbits
      v = p.psvoid
   in inr b 0 1
        && inr i 0 1
        && inr s 0 1
        && inr c 0 1
        && inr l 0 2 -- long and long long
        && inr f 0 1
        && inr d 0 1
        && inr o 0 1
        && inr m 0 1
        && (e `elem` [0, 32, 64, 128])
        && inr v 0 1

psanyfloat :: PTSummary -> Bool
psanyfloat p =
  (psfloat p > 0)
    || (psdouble p > 0)
    || (pscomplex p > 0)
    || (psdecimal p > 0)
    || (psdecimalbits p > 0)

psanyint :: PTSummary -> Bool
psanyint p = i > 0 || s > 0 || c > 0 || l > 0
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
  | not (psbasiccheck p) = throwbasic "pscvt: basic check failed"
  | psvoid p > 0 =
      if psanyint p || psanyfloat p
        then throwbasic "pscvt: void and other types"
        else
          if p.pssigned
            then throwbasic "pscvt: signed or unsigned void"
            else pure Void
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
        else pure $ Int (pscoercesign p) BitInt
  | psint p > 0 =
      if ps.anyfloat p
        then throwbasic "pscvt: int and other types"
        else pure $ Int (pscoercesign p) IntType
  | psshort p > 0 =
      if ps.anyfloat p
        then throwbasic "pscvt: short and other types"
        else pure $ Int (pscoercesign p) Short
  | pslong p > 1 -- long long
    =
      if ps.anyfloat p
        then throwbasic "pscvt: long long and other types"
        else pure $ Int (pscoercesign p) LongLong
  | psfloat p > 0 =
      if p.psdouble > 0 || p.psdecimal > 0 || p.psdecimalbits > 0
        then throwbasic "pscvt: float and other types"
        else
          if p.psdouble > 0
            then pure $ Float (Complex RFFloat)
            else pure $ Float (Real RFFloat)
  | psdouble p > 0 =
      if p.psfloat > 0 || p.psdecimal > 0 || p.psdecimalbits > 0
        then throwbasic "pscvt: double and other types"
        else
          let subty
                | p.pslong == 0 = RFDouble
                | p.pslong == 1 = RFLongDouble
                | otherwise = throwbasic "pscvt: long long double"
           in if p.pscomplex > 0
                then pure $ Float (Complex subty)
                else pure $ Float (Real subty)
  | pslong p > 0 =
      if ps.anyfloat p
        then throwbasic "pscvt: long and other types"
        else pure $ Int (pscoercesign p) Long
  | psdecimal p > 0 =
      if p.pscomplex > 0
        then throwbasic "pscvt: decimal complex"
        else pure $ Float (Decimal RFFloat)
  | otherwise =
      throwbasic
        $ "pscvt: unsupported or empty type summary"
        ++ show p

constexpr = error "constexpr: not implemented"
