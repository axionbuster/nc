{-# OPTIONS_GHC -Wno-name-shadowing #-}

module NC.Parser where

import Control.Lens
import Control.Monad
import Control.Monad.Fix (fix)
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Coerce
import Data.Functor
import Data.Int
import Debug.Trace qualified as Tr
import FlatParse.Stateful
import Text.Printf
import UnliftIO.IORef
import Prelude

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
  | -- | @expected ...@
    MsgExpect String
  deriving (Eq)

instance Show Message where
  showsPrec = prettymsg

prettymsg :: Int -> Message -> ShowS
prettymsg d =
  showParen (d > 10) . \case
    MsgAdHoc s -> (s ++)
    MsgExpect s -> ("expected " ++) . showsPrec 11 s

-- | Message with annotations.
data AMessage = AMessage
  { -- | What message?
    _am_msg :: !Message,
    -- | Who's sending this message?
    _am_who :: !String,
    -- | This message is caused by ...
    _am_why :: !(Maybe AMessage),
    -- | What point location?
    _am_pos :: !Pos,
    -- | If it recovered, from which location?
    _am_spn :: !(Maybe Span64)
  }
  deriving (Eq, Show)

makeLenses ''AMessage

-- | Our parsing environment.
data PEnv = PEnv
  { -- | A LIFO stack of messages.
    _penv_messages :: !(IORef [AMessage]),
    -- | Original string. This is needed for error reporting.
    _penv_original :: !ByteString
  }

-- | Our parser type.
type P = ParserIO PEnv AMessage

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

-- | Annotate and then throw a 'Message'. One must clarify \'who\' is sending
-- the message.
pthrow :: Message -> String -> P a
pthrow m w = do
  -- At first we have a point-like error.
  s <- getPos
  err $ AM0 m w s

-- | If the parser fails throw the message. If we get an error instead,
-- cite the error as the cause and expand the span as needed.
pcut :: P a -> Message -> String -> P a
pcut p m w = do
  s <- getPos
  cutting p (AM0 m w s) \inner _ ->
    AMessage m w (Just inner) s Nothing

-- | If it throws an error, log it and then fail.
ptry :: P a -> P a
ptry p = do
  p `withError` \e -> do
    msgs <- _penv_messages <$> ask
    modifyIORef' msgs (e :)
    failed

-- | If @p@ throws an error, log it. Then, continue until @q@ fails. Resume
-- with @r@ from then on.
psync :: P a -> P () -> P a -> P a
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

-- | Flipped version of 'pcut'.
pcut' :: Message -> String -> P a -> P a
pcut' m w p = pcut p m w

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
  msgs <- liftIO $ readIORef (env._penv_messages)
  traceIO $ printf "_dbg_dumpmsgs\n"

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
  let msg = case amsg._am_msg of
        MsgAdHoc s -> s
        MsgExpect s -> "expected " ++ s
      who = amsg._am_who
      indentation = replicate indent ' '
      related =
        maybe
          ""
          (\m -> "\n" ++ indentation ++ "↳ " ++ _dbg_formatamsg (indent + 2) m)
          amsg._am_why
   in indentation ++ "from " ++ who ++ ": " ++ msg ++ related
