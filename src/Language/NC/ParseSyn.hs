-- due to a defect in FlatParse TH splice (generation of 'anykeyword')
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Language.NC.ParseSyn where

import Data.ByteString.Char8 qualified as C
import Language.NC.Prelude

-- in this temporarily small module, we'll be lexing and parsing together.

-- * Lexing

-- honestly, doing this for ByteString parsing is kind of tedious.
-- there has to be a better way ... for less-hot places, probably
-- ok to use regular strings which are much nicer to work with.
ident :: Parser ByteString
ident = do
  n <- lookahead mainparse
  (isolate (C.length n) anykeyword `fails`) $> n
  where
    mainparse = C.cons <$> ident_h <*> ident_t
    ident_h = satisfy \c -> isLatinLetter c || c == '_'
    ident_t = byteStringOf $
      skipMany $
        satisfy \c -> isLatinLetter c || c == '_' || isDigit c

-- match any keyword (reserved word)
anykeyword :: Parser ()
anykeyword =
  $( switch
       [|
         case _ of
           "auto" -> pure ()
           "break" -> pure ()
           "case" -> pure ()
           "char" -> pure ()
           "const" -> pure ()
           "continue" -> pure ()
           "default" -> pure ()
           "do" -> pure ()
           "double" -> pure ()
           "else" -> pure ()
           "enum" -> pure ()
           "extern" -> pure ()
           "float" -> pure ()
           "for" -> pure ()
           "goto" -> pure ()
           "if" -> pure ()
           "inline" -> pure ()
           "int" -> pure ()
           "long" -> pure ()
           "register" -> pure ()
           "restrict" -> pure ()
           "return" -> pure ()
           "short" -> pure ()
           "signed" -> pure ()
           "sizeof" -> pure ()
           "static" -> pure ()
           "struct" -> pure ()
           "switch" -> pure ()
           "typedef" -> pure ()
           "union" -> pure ()
           "unsigned" -> pure ()
           "void" -> pure ()
           "volatile" -> pure ()
           "while" -> pure ()
           "_Bool" -> pure ()
           "_Complex" -> pure ()
           "_Imaginary" -> pure ()
           _ -> failed
         |]
   )
