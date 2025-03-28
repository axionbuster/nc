-- | Parser type, error type
module Language.NC.Internal.Parse
  ( Parser,
    ParserState (..),
    WithSpan (..),
    Symbol (..),
    IntegerSettings (..),
    CharSettings (..),
    Str,
    Seq (..),
    RelatedInfo (..),
    pwithspan,
    cut,
    mkstate0,
    throwbasic,
    ist_preciseposbw,
    ist_precisebw,
    int_canrepresent,
  )
where

import Data.ByteString.Short
import Data.IORef
import Data.Sequence (Seq ((:<|), (:|>)))
import Data.Word
import FlatParse.Stateful hiding (Parser)
import Language.NC.Internal.Error
import Language.NC.Internal.PrimTypes qualified as PT
import Prelude

-- | Unpinned memory used for short strings.
type Str = ShortByteString

-- | Symbol
data Symbol = Symbol
  deriving (Eq, Show)

-- | Bit widths of primitive integers. Pay especially close attention to
-- the bit width of @long@; on Windows, it's generally 32 bits, but on
-- other platforms, it's generally 64 bits.
--
-- FIXME: 'IntegerSettings' is also responsible for floating-point settings.
data IntegerSettings
  = IntegerSettings
  { ist_charbitwidth :: !Word8,
    ist_shortbitwidth :: !Word8,
    ist_intbitwidth :: !Word8,
    ist_longbitwidth :: !Word8,
    ist_longlongbitwidth :: !Word8,
    -- | is @char@ represented as @signed char@ or @unsigned char@?
    ist_charissigned :: !Bool,
    ist_floatbitwidth :: !Word8,
    ist_doublebitwidth :: !Word8,
    ist_longdoublebitwidth :: !Word8
  }
  deriving (Eq, Show)

ain :: Parser IntegerSettings
ain = psintset <$> ask

-- | Find the precise bit width needed to represent a positive number.
--
-- Signed integers have one fewer bit than the unsigned counterpart.
--
-- For @char@, it depends on the signedness.
ist_preciseposbw :: PT.PrimType -> Parser Word8
ist_preciseposbw (PT.Int _ (PT.BitInt n)) = pure n
ist_preciseposbw (PT.Int s a) = do
  x <- ist_pbw' a
  pure $ x - fromIntegral (fromEnum (s == PT.Signed))
  where
    ist_pbw' PT.Short = ist_shortbitwidth <$> ain
    ist_pbw' PT.IntLen = ist_intbitwidth <$> ain
    ist_pbw' PT.Long = ist_longbitwidth <$> ain
    ist_pbw' PT.LongLong = ist_longlongbitwidth <$> ain
    ist_pbw' _ = error "ist_pbw' on _BitInt(N) ... impossible"
ist_preciseposbw (PT.Char (Just PT.Unsigned)) = ist_charbitwidth <$> ain
ist_preciseposbw (PT.Char (Just PT.Signed)) = pred . ist_charbitwidth <$> ain
ist_preciseposbw (PT.Char Nothing) = do
  csg <- ist_charissigned <$> ain
  if csg
    then pred . ist_charbitwidth <$> ain
    else ist_charbitwidth <$> ain
ist_preciseposbw (PT.Float f)
  | PT.Complex a <- f = (2 *) <$> ist_preciseposbw (PT.Float $ PT.Real a)
  | PT.Real PT.RFFloat <- f = ist_floatbitwidth <$> ain
  | PT.Real PT.RFDouble <- f = ist_doublebitwidth <$> ain
  | PT.Real PT.RFLongDouble <- f = ist_longdoublebitwidth <$> ain
  | PT.Real PT.RFDecimal128 <- f = pure 128
  | PT.Real PT.RFDecimal32 <- f = pure 32
  | PT.Real PT.RFDecimal64 <- f = pure 64
ist_preciseposbw t =
  err $
    InternalError $
      "ist_preciseposbw called on unsupported type " ++ show t

-- | Recall the exact number of bits needed to represent a scalar type.
ist_precisebw :: PT.PrimType -> Parser Word8
ist_precisebw (PT.Int _ a) = case a of
  PT.Short -> ist_shortbitwidth <$> ain
  PT.IntLen -> ist_intbitwidth <$> ain
  PT.Long -> ist_longbitwidth <$> ain
  PT.LongLong -> ist_longlongbitwidth <$> ain
  PT.BitInt n -> pure n
ist_precisebw (PT.Char _) = ist_charbitwidth <$> ain
ist_precisebw t = ist_preciseposbw t

-- | Currently, the real type that is equal to @wchar_t@.
data CharSettings = CharSettings
  { -- | We'll make @wchar_t@ unsigned for many reasons.
    cst_wchar_type :: PT.PrimType,
    -- | It's good to make @char8_t@ unsigned.
    cst_char8_type :: PT.PrimType,
    -- | must be unsigned.
    cst_char16_type :: PT.PrimType,
    -- | must be unsigned, too.
    cst_char32_type :: PT.PrimType
  }

-- | Parsing state, to include such things as symbol tables.
data ParserState = ParserState
  { pserrors :: IORef (Seq AnnotatedError),
    psintset :: IntegerSettings,
    pscharset :: CharSettings
  }

-- | See if an integer constant can be represented by a given type.
int_canrepresent :: Integer -> PT.PrimType -> Parser Bool
int_canrepresent i = \case
  PT.Bool -> pure $ i == 0 || i == 1
  t@(PT.Int PT.Signed _; PT.Char (Just PT.Signed)) -> rep PT.Signed t
  t@(PT.Int PT.Unsigned _; PT.Char (Just PT.Unsigned)) -> rep PT.Unsigned t
  t@(PT.Char Nothing) -> do
    s <- ist_charissigned <$> ain
    rep (if s then PT.Signed else PT.Unsigned) t
  _ -> err (InternalError "int_canrepresent called on a non-integral type")
  where
    rep PT.Signed t = do
      bw <- ist_precisebw t
      let rngtop = 2 ^ (bw - 1) - 1
      let rngbot = negate $ 2 ^ (bw - 1)
      pure $ rngbot <= i && i <= rngtop
    rep PT.Unsigned t = do
      bw <- ist_precisebw t
      pure $ i <= 2 ^ bw

-- | Create a starter state.
--
-- Defaults: these defaults may work better on a UNIX-like system.
-- They definitely don't work on Windows.
--
-- - @long@ is assigned 64 bits as a temporary measure.
-- - @char@ is backed by @signed char@.
-- - @wchar_t@ is currently represented by @int@.
mkstate0 :: IO ParserState
mkstate0 = do
  e <- newIORef mempty
  let is0 = IntegerSettings 8 16 32 64 64 True 32 64 64
  let cs0 = CharSettings PT.UInt_ PT.UChar_ PT.UShort_ PT.UInt_
  pure (ParserState e is0 cs0)

-- | The parser, which lives in IO.
type Parser = ParserIO ParserState Error

-- | Data with span.
data WithSpan a = WithSpan Span a
  deriving (Eq, Show)

-- | Pure \"parser\" to return a 'WithSpan'.
--
-- Argument order was readjusted to agree with 'withSpan' in "flatparse".
pwithspan :: a -> Span -> Parser (WithSpan a)
pwithspan = (pure .) . flip WithSpan

-- | Throw a 'BasicError'.
throwbasic :: String -> Parser a
throwbasic = err . BasicError
