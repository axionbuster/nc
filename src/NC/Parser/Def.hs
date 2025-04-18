{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- |
-- Module: NC.Parser.Def
-- Description: Define the parser, error handling, etc.
module NC.Parser.Def (
  -- * Defining the parser
  P,
  PEnv (..),
  Settings (..),
  SizeSettings (..),
  EncodingSettings (..),
  penv_messages,
  penv_original,
  penv_settings,
  penv_psym,
  szs_boolbits,
  szs_charbits,
  szs_charptrbits,
  szs_doublebits,
  szs_floatbits,
  szs_intbits,
  szs_longbits,
  szs_longlongbits,
  szs_longdoublebits,
  szs_shortbits,
  encset_charissigned,
  encset_wchartype,
  encset_char8type,
  encset_char16type,
  encset_char32type,
  setg_encoding,
  setg_sizes,
  am_msg,
  am_pos,
  am_spn,
  am_who,
  am_why,
  sizesettings,
  posbwof,

  -- * Symbols and lookups
  Str,
  Lookup (..),
  PNS (..),
  PSym,
  symnew,
  symgivetypetag,
  symgivegeneralname,

  -- * Span
  Span64 (..),
  i32topos,
  postoi32,
  span64,
  sp_begin_pos,
  sp_end_pos,

  -- * Message production and error handling.
  Message (..),
  AMessage (..),
  oops,
  adhoc,
  pthrow,
  pcutfull,
  pcut,
  pcut',
  ptry,
  pcut_expect,
  psync,
  ponexception,
  pfinally,
  pexpect,

  -- * Debugging.
  _dbg_dumpmsgs,
) where

import Control.Lens
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Coerce
import Data.Functor
import Data.Int
import Data.Unique
import Data.Word
import Debug.Trace qualified as Tr
import FlatParse.Stateful
import NC.Type.Def
import NC.Type.Prim
import Text.Printf
import UnliftIO.IORef
import Prelude

-- | Shared string type. FlatParse uses a single 'ByteString' and so
-- any excerpts will also be shared with that buffer.
type Str = ByteString

-- | Parsing settings.
data Settings = Settings
  { _setg_sizes :: SizeSettings,
    _setg_encoding :: EncodingSettings
  }

-- | Our parsing environment.
data PEnv = PEnv
  { -- | A LIFO stack of messages.
    _penv_messages :: !(IORef [AMessage]),
    -- | Settings for various pluggable behaviors.
    _penv_settings :: !Settings,
    -- | Original string. This is needed for error reporting.
    _penv_original :: !ByteString,
    -- | Symbol tables.
    _penv_psym :: !PSym
  }

-- | Stack of symbol tables.
type PSym = [PNS]

-- | Result of a misc. symbol lookup.
data Lookup
  = LFunction
  | LObject
  | LTypedef
  | LEnumConstant
  deriving (Eq, Show)

-- | Parser namespace
data PNS = PNS
  { _pns_tags :: Table Str Type,
    _pns_others :: Table Str Lookup
  }

-- | Our parser type.
type P = ParserIO PEnv AMessage

-- | Determine the sizes of types.
data SizeSettings
  = IntegerSettings
  { _szs_boolbits :: !Word8,
    _szs_charbits :: !Word8,
    _szs_shortbits :: !Word8,
    _szs_intbits :: !Word8,
    _szs_longbits :: !Word8,
    _szs_longlongbits :: !Word8,
    _szs_floatbits :: !Word8,
    _szs_doublebits :: !Word8,
    _szs_longdoublebits :: !Word8,
    -- | 7.21.2.3: "The size and alignment of @nullptr\_t@ is the same as
    -- for a pointer to character type"
    _szs_charptrbits :: !Word8
  }
  deriving (Eq, Show)

data EncodingSettings
  = EncodingSettings
  { _encset_charissigned :: !Bool,
    _encset_wchartype :: !Prim,
    _encset_char8type :: !Prim,
    _encset_char16type :: !Prim,
    _encset_char32type :: !Prim
  }
  deriving (Eq, Show)

traceIO :: (MonadIO m) => String -> m ()
traceIO = liftIO . Tr.traceIO

-- | A variation on 'FlatParse.Stateful.Span' that fits neatly in 64 bits.
data Span64
  = Span64
      -- | begin
      {-# UNPACK #-} !Int32
      -- | end
      {-# UNPACK #-} !Int32
  deriving (Eq, Show)

i32 :: Int32 -> Pos
i32 = coerce @Int @Pos . fromIntegral

pos :: Pos -> Int32
pos = fromIntegral . coerce @Pos @Int

-- | Convert from an 'Int32' to a 'Pos'
i32topos :: Int32 -> Pos
i32topos = i32

-- | Convert from a 'Pos' to an 'Int32'
postoi32 :: Pos -> Int32
postoi32 = pos

-- | Convert between 'Span64' and 'FlatParse.Stateful.Span'. It is assumed that
-- 'Pos' will never exceed what fits in a positive signed 'Int32' value.
-- This is probably true because we won't be parsing a 2.1 GB C source file.
span64 :: Iso' Span64 Span
span64 = iso getter setter
 where
  getter (Span64 s f) = Span (i32 s) (i32 f)
  setter (Span s f) = Span64 (pos s) (pos f)

sp_begin_pos, sp_end_pos :: Lens' Span64 Pos

-- | Recall the beginning position.
sp_begin_pos = lens getter setter
 where
  getter (Span64 s _) = i32 s
  setter (Span64 _ e) s = Span64 (pos s) e

-- | Recall the ending position.
sp_end_pos = lens getter setter
 where
  getter (Span64 _ e) = i32 e
  setter (Span64 s _) e = Span64 s (pos e)

-- | A bare-bones message type. This isn't the place to store annotations
-- and other information.
data Message
  = -- | an \'on-the-spot\' message
    MsgAdHoc String
  | -- | an internal error message
    MsgOops String
  | -- | @expected ...@
    MsgExpect String
  deriving (Eq)

instance Show Message where
  showsPrec = prettymsg

-- | Message with annotations.
data AMessage = AMessage
  { -- | What message?
    _am_msg :: !Message,
    -- | Who's sending this message, if any? Empty string to signal
    -- no particular sender specified.
    _am_who :: !String,
    -- | This message is caused by ...
    _am_why :: !(Maybe AMessage),
    -- | What point location?
    _am_pos :: !Pos,
    -- | If it recovered, from which location?
    _am_spn :: !(Maybe Span64)
  }
  deriving (Eq, Show)

prettymsg :: Int -> Message -> ShowS
prettymsg d =
  showParen (d > 10) . \case
    MsgAdHoc s -> (s ++)
    MsgOops s -> ("internal error: " ++) . showsPrec 11 s
    MsgExpect s -> ("expected " ++) . showsPrec 11 s

makeLenses ''PEnv

makeLenses ''SizeSettings

makeLenses ''EncodingSettings

makeLenses ''Settings

makeLenses ''AMessage

-- | (I) Create a properly ordered 'Span64'.
_mk64 :: Pos -> Pos -> Span64
_mk64 a b
  | a <= b = Span64 (pos a) (pos b)
  | otherwise = Span64 (pos b) (pos a)

-- | (I) Makes throwing point (1-char span, inclusively counted) errors easier.
pattern AM0 :: Message -> String -> Pos -> AMessage
pattern AM0 m w p <- AMessage m w _ p _
  where
    AM0 m w p = AMessage m w Nothing p Nothing

-- | Stretch out the position to include the new span.
_stretchspan64 :: Pos -> Span64 -> Span64
_stretchspan64 (fromIntegral . coerce @Pos @Int -> p) (Span64 a b)
  | p < a = Span64 p b
  | b < p = Span64 a p
  | otherwise = Span64 a b

-- | Throw an internal compiler error.
oops :: String -> String -> P a
oops = pthrow . MsgOops

-- | Throw an ad hoc error. Message then sender identification.
adhoc :: String -> String -> P a
adhoc = pthrow . MsgAdHoc

-- | Annotate and then throw a 'Message'. One must clarify \'who\' is sending
-- the message.
pthrow :: Message -> String -> P a
pthrow m w = do
  -- At first we have a point-like error.
  s <- getPos
  err $ AM0 m w s

-- | If the parser fails throw the message. If we get an error instead,
-- cite the error as the cause and expand the span as needed.
pcutfull :: P a -> Message -> String -> P a
pcutfull p m w = do
  s <- getPos
  cutting p (AM0 m w s) \inner _ ->
    AMessage m w (Just inner) s Nothing

-- | If the parser fails, throw the message, but without sender attribution.
-- See 'pcutfull'.
pcut :: P a -> Message -> P a
pcut p m = pcutfull p m ""

-- | Flipped version of 'pcut'.
pcut' :: Message -> P a -> P a
pcut' m p = pcut p m

-- | If it throws an error, log it and then fail.
ptry :: P a -> P a
ptry p = do
  p `withError` \e -> do
    msgs <- _penv_messages <$> ask
    modifyIORef' msgs (e :)
    failed

-- | Cut with 'MsgExpect'.
pcut_expect :: P a -> String -> P a
pcut_expect p = pcut p . MsgExpect

-- | Emit a message saying that something is expected without
-- sender attribution.
pexpect :: String -> P a
pexpect m = pthrow (MsgExpect m) ""

-- | If @p@ throws an error, log it. Then, continue until @q@ fails. Resume
-- with @r@ from then on.
psync :: P a -> P b -> P a -> P a
psync p q r = do
  p `withError` \e -> do
    skipMany q
    s <- getPos
    let e' =
          e & over am_spn \case
            Nothing -> Just $ _mk64 (e._am_pos) s
            Just t -> Just $ _stretchspan64 s t
    msgs <- _penv_messages <$> ask
    modifyIORef' msgs (e' :)
    r

-- | If there's an error, perform the second action and then rethrow
-- the same error. Otherwise, don't do anything special.
ponexception :: P a -> P b -> P a
ponexception p q = p `withError` \e -> q >> err e

-- | Attempt to parse something. Whether it errors out or not, do
-- the specified action. Then, rethrow the exception, if raised.
pfinally :: P a -> P b -> P a
pfinally p q = do a <- p `ponexception` q; q $> a

-- | Dump messages to the console.
--
-- This is based on the old @dbg_dumperrs@ function but adapted for the new
-- message structure.
_dbg_dumpmsgs :: ByteString -> P ()
_dbg_dumpmsgs orig = do
  env <- ask
  msgs <- readIORef env._penv_messages
  traceIO $ printf "_dbg_dumpmsgs"

  forM_ msgs \amsg -> do
    let msg = show amsg
        who = amsg._am_who
        pos = amsg._am_pos
        position = case amsg._am_spn of
          Nothing -> (pos, pos) -- point error
          Just (Span64 s e) -> (i32topos s, i32topos e) -- span error

    -- Get line/column information
    let positions = [fst position, snd position]
        -- FlatParse line/col are zero-based, so we add 1 to make them 1-based
        strposes = posLineCols orig positions <&> bimap succ succ
        ((sl, sc), (el, ec)) = case strposes of
          [start, end] -> (start, end)
          _ -> ((0, 0), (0, 0)) -- fallback

    -- Get the lines containing the error
    setPos (fst position)
    ls <- fix \r -> do
      here <- getPos
      branch
        eof
        (pure ["<EOF>"])
        ( if here <= snd position
            then (:) <$> takeLine <*> r
            else pure []
        )

    traceIO $
      printf
        """
        message: %s
        from: %s
        position: (%v:%v)-(%v:%v)
        related: %s
        listing:
        %s

        """
        msg
        who
        sl
        sc
        el
        ec
        (maybe "<nothing>" (_dbg_formatamsg 2) amsg._am_why)
        (unlines ls)

-- Helper function to format nested messages with indentation
_dbg_formatamsg :: Int -> AMessage -> String
_dbg_formatamsg indent amsg =
  let msg = show amsg
      who = amsg._am_who
      indentation = replicate indent ' '
      related =
        maybe
          ""
          (\m -> "\n" ++ indentation ++ "↳ " ++ _dbg_formatamsg (indent + 2) m)
          amsg._am_why
   in indentation ++ "from " ++ who ++ ": " ++ msg ++ related

sizesettings :: P SizeSettings
sizesettings = _setg_sizes . _penv_settings <$> ask

posbwof_real :: PrimSignedInteger -> P BitSize
posbwof_real = \case
  PSI SICBitInt _ bw -> pure bw
  PSI categ Signed bw -> posbwof_real (PSI categ Unsigned bw) <&> subtract 1
  PSI categ _ _ -> case categ of
    SICChar -> gg _szs_charbits
    SICShort -> gg _szs_shortbits
    SICInt -> gg _szs_intbits
    SICLong -> gg _szs_longbits
    SICLongLong -> gg _szs_longlongbits
   where
    gg h = fromIntegral @Word8 @Word16 . h <$> sizesettings

-- | Find the number of consecutively laid out bits needed to represent
-- the largest positive number representable in each type.
-- This normally only makes sense for integral types, though.
posbwof :: Prim -> P BitSize
posbwof (view pr_info -> info) = case info ^? pi_si of
  Just psi -> posbwof_real psi
  Nothing -> case info of
    PrimNSChar -> do
      cb <- gg _szs_charbits
      cs <- ask <&> view encset_charissigned . _setg_encoding . _penv_settings
      cb & pure . if cs then subtract 1 else id
    PrimFloat CxComplex float -> casefloat float <&> subtract 1 . (* 2)
    PrimFloat _ float -> casefloat float <&> subtract 1
    PrimNullptr -> gg _szs_charptrbits
    PrimBool -> gg _szs_boolbits
    _ -> pure 0
   where
    gg h = fromIntegral @Word8 @Word16 . h <$> sizesettings
    casefloat = \case
      FCFloat -> gg _szs_floatbits
      FCDouble -> gg _szs_doublebits
      FCLongDouble -> gg _szs_longdoublebits
      FCDecimal32 -> pure 32
      FCDecimal64 -> pure 64
      FCDecimal128 -> pure 128

-- | Create a fresh unnamed symbol.
symnew :: (MonadIO m) => m Symbol
symnew = liftIO (coerce newUnique)

-- | Give the symbol a type tag.
symgivetypetag :: Symbol -> Name -> P ()
symgivetypetag = undefined

-- | Give the symbol a general name (e.g., variables, functions, etc.).
symgivegeneralname :: Symbol -> Name -> P ()
symgivegeneralname = undefined
