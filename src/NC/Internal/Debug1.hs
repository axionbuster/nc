-- | Module for interactive debugging
module NC.Internal.Debug1 (
  module NC.Internal.Prelude1,
  mkenv,
  run,
  runok,
) where

import Data.HashTable.IO qualified as H
import Data.List.NonEmpty (NonEmpty (..))
import NC.Internal.Prelude1

-- | Construct a debugging environment
mkenv :: Str -> IO PEnv
mkenv s = do
  msgs <- newIORef []
  let settings = Settings sz enc
       where
        sz =
          SizeSettings
            { _szs_shortbits = 16,
              _szs_longlongbits = 64,
              _szs_longdoublebits = 64,
              _szs_longbits = 64,
              _szs_intbits = 32,
              _szs_floatbits = 32,
              _szs_doublebits = 64,
              _szs_charptrbits = 64,
              _szs_charbits = 8,
              _szs_boolbits = 8
            }
        enc =
          EncodingSettings
            { _encset_wchartype = PrimUShort,
              _encset_charissigned = False,
              _encset_char8type = PrimUChar,
              _encset_char32type = PrimUInt,
              _encset_char16type = PrimUShort
            }
  sym2type <- H.new
  -- Create initial scope for the stack
  tags0 <- H.new
  others0 <- H.new
  let initialScope = PNS tags0 others0
  stack <- newIORef (initialScope :| [])
  let psym = PSym sym2type stack
  pure $ PEnv msgs settings s psym

-- | Run a parser
run :: P a -> String -> IO (R a)
run p s = do
  let t = strToUtf8 s
  st <- mkenv t
  runParserIO (ws0 >> p) st 0 t

-- | Run a parser and then ...
--
-- - If it's successful, return the parsed value.
-- - If it fails, fail likewise ('mzero').
-- - If it throws an error, 'show' the error in an ErrorCall.
runok :: P a -> String -> IO a
runok p s =
  run p s >>= \case
    OK a _ _ -> pure a
    Fail -> mzero
    Err e -> error $ show e
