-- | Parse primitive non-derived types.
module Language.NC.Parse.PrimTypes (primtype) where

import Language.NC.Experiment.Types
import Language.NC.Internal.Prelude
import Language.NC.Lex2

-- i will parse PT and then merge it into a PrimType value
-- that's being unfolded. so a PT represents a token and
-- it essentially gets left-folded onto a PTSummary value.
-- the PTSummary value is then used to determine the final
-- PrimType value. to reduce backtracking in my PEG parser
-- i decided to use counting instead of a permutation parser.
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
  | PTBitInt Word8 -- bit width

-- record counts of each token occurrence
data PTSummary
  = PTSummary
  { pssigned :: Maybe Signed,
    pssigns :: Word8,
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

instance Show PTSummary where
  show PTSummary {..} = do
    let show' n s
          | n > 1 = "(multiple " ++ s ++ " specifiers)"
          | n > 0 = s
          | otherwise = ""
    let ww =
          ( if pssigns > 1
              then "(multiple sign specifiers)"
              else case pssigned of
                Just Signed -> "signed"
                Just Unsigned -> "unsigned"
                Nothing -> ""
          )
            : show' psvoid "void"
            : ( case psbitint of
                  0 -> ""
                  1 -> "_BitInt(" ++ show psbitintwidth ++ ")"
                  _ -> "(multiple _BitInt specifiers)"
              )
            : show' psshort "short"
            : (if pslong > 1 then "long long" else show' pslong "long")
            : show' psint "int"
            : show' pschar "char"
            : show' psfloat "float"
            : show' psdouble "double"
            : ( case psdecimal of
                  0 -> ""
                  1 -> "_Decimal" ++ show psdecimalbits
                  _ -> "(multiple _Decimal specifiers)"
              )
            : show' pscomplex "_Complex"
            : []
    unwords $ filter (not . null) ww

psbasiccheck :: PTSummary -> Bool
psbasiccheck p =
  (p.psbitint <= 1)
    && (p.pssigns <= 1)
    && (p.psint <= 1)
    && (p.psshort <= 1)
    && (p.pschar <= 1)
    && (p.pslong <= 2) -- long and long long
    && (p.psfloat <= 1)
    && (p.psdouble <= 1)
    && (p.pscomplex <= 1)
    && (p.psdecimal <= 1)
    && (p.psvoid <= 1)

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

-- C primitive type keywords can be given in any permutation
-- but i didn't want to use a permutation parser; so this is
-- an implementation that's based on counting keywords instead.
pscvt :: PTSummary -> Parser PrimType
pscvt p
  | not (psbasiccheck p) =
      emit (InvalidTypeSpec $ show p)
  | psvoid p > 0 =
      if psanyint p || psanyfloat p
        then emit (IncompatibleTypes "void" [])
        else case p.pssigned of
          Just _ -> emit (InvalidSignedness "void")
          Nothing -> pure Void
  | pschar p > 0 =
      if (p.psbitint > 0)
        || (p.psint > 0)
        || (p.psshort > 0)
        || (p.pslong > 0)
        || psanyfloat p
        then emit (IncompatibleTypes "char" [])
        else pure $ Char (pssigned p)
  | psbitint p > 0 =
      if (p.psint > 0)
        || (p.psshort > 0)
        || (p.pslong > 0)
        || psanyfloat p
        then emit (IncompatibleTypes "_BitInt" [])
        else
          if p.psbitintwidth == 0
            then emit (InvalidBitIntWidth 0)
            else
              pure
                $ Int (pscoercesign p)
                $ BitInt
                $ fromIntegral p.psbitintwidth
  | psint p > 0 =
      if psanyfloat p || (p.psshort > 0 && p.pslong > 0)
        then emit (IncompatibleTypes "int" [])
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
        then emit (IncompatibleTypes "short" [])
        else pure $ Int (pscoercesign p) Short
  | pslong p > 1 -- long long
    =
      if psanyfloat p
        then emit (IncompatibleTypes "long long" [])
        else pure $ Int (pscoercesign p) LongLong
  | psfloat p > 0 =
      if (p.psdouble > 0)
        || (p.psdecimal > 0)
        || (p.psdecimalbits > 0)
        || (p.pslong > 0)
        then emit (IncompatibleTypes "float" [])
        else
          if p.pssigned /= Nothing
            then emit (InvalidSignedness "float")
            else
              if p.pscomplex > 0
                then pure $ Float (Complex RFFloat)
                else pure $ Float (Real RFFloat)
  | psdouble p > 0 =
      if p.psfloat > 0 || p.psdecimal > 0 || p.psdecimalbits > 0
        then emit (IncompatibleTypes "double" [])
        else
          if p.pssigned /= Nothing
            then emit (InvalidSignedness "double")
            else do
              subty <- case p.pslong of
                0 -> pure RFDouble
                1 -> pure RFLongDouble
                _ -> emit (IncompatibleTypes "long long" [])
              if p.pscomplex > 0
                then pure $ Float (Complex subty)
                else pure $ Float (Real subty)
  | pslong p > 0 =
      if psanyfloat p
        then emit (IncompatibleTypes "long" [])
        else pure $ Int (pscoercesign p) Long
  | psdecimal p > 0 =
      if p.pscomplex > 0
        then emit InvalidDecimalComplex
        else
          if p.pssigned /= Nothing
            then emit (InvalidSignedness "_Decimal")
            else case p.psdecimalbits of
              32 -> pure $ Float (Real RFDecimal32)
              64 -> pure $ Float (Real RFDecimal64)
              128 -> pure $ Float (Real RFDecimal128)
              _ -> emit (InvalidDecimalBits p.psdecimalbits)
  | otherwise = emit (InvalidTypeSpec $ show p)
  where
    emit = err . PrimTypeBadError

inc :: Word8 -> Word8
inc w | w == maxBound = maxBound | otherwise = w + 1

psfold :: PTSummary -> PT -> PTSummary
psfold !sm = \case
  CarrySign s -> sm {pssigned = Just s, pssigns = inc $ pssigns sm}
  CarryInt i -> case i of
    PTChar -> sm {pschar = inc $ pschar sm}
    PTShort -> sm {psshort = inc $ psshort sm}
    PTInt -> sm {psint = inc $ psint sm}
    PTLong -> sm {pslong = inc $ pslong sm}
    PTBitInt w -> sm {psbitint = inc $ psbitint sm, psbitintwidth = w}
  CarryFloat f -> case f of
    RFFloat -> sm {psfloat = inc $ psfloat sm}
    RFDouble -> sm {psdouble = inc $ psdouble sm}
    RFLongDouble -> sm {psdouble = inc $ psdouble sm}
    RFDecimal32 -> sm {psdecimal = inc $ psdecimal sm, psdecimalbits = 32}
    RFDecimal64 -> sm {psdecimal = inc $ psdecimal sm, psdecimalbits = 64}
    RFDecimal128 -> sm {psdecimal = inc $ psdecimal sm, psdecimalbits = 128}
  CarryComplex -> sm {pscomplex = inc $ pscomplex sm}
  CarryChar -> sm {pschar = inc $ pschar sm}
  CarryVoid -> sm {psvoid = inc $ psvoid sm}

ptkeyword :: Parser PT
ptkeyword = do
  lex
    $ choice
      [ int' $> CarryInt PTInt,
        void' $> CarryVoid,
        char' $> CarryInt PTChar,
        long' $> CarryInt PTLong,
        double' $> CarryFloat RFDouble,
        float' $> CarryFloat RFFloat,
        signed' $> CarrySign Signed,
        unsigned' $> CarrySign Unsigned,
        short' $> CarryInt PTShort,
        _Complex' $> CarryComplex,
        bitint,
        _Decimal32' $> CarryFloat RFDecimal32,
        _Decimal64' $> CarryFloat RFDecimal64,
        _Decimal128' $> CarryFloat RFDecimal128
      ]
  where
    bitint = do
      lex _BitInt'
      cut
        ( inpar
            $ lex
            $ CarryInt
            . PTBitInt
            . fromIntegral
            <$> (anyAsciiDecimalInt >>= guardnat)
        )
        (PrimTypeBadError InvalidBitIntWidthOverflowOrBadFormat)
    guardnat n = guard (n >= 0 && n < 256) $> n

-- | Parse a primitive, non-derived type
primtype :: Parser PrimType
primtype =
  chainl
    psfold
    (pure $ PTSummary Nothing 0 0 0 0 0 0 0 0 0 0 0 0 0)
    ptkeyword
    >>= pscvt
