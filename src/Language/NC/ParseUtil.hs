module Language.NC.ParseUtil where

import Control.Monad ((>>))
import Data.ByteString (ByteString, toStrict)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BB
import Data.Char (Char)
import Data.Functor (($>))
import FlatParse.Stateful hiding (Parser)
import Language.NC.Parse
import Prelude (($), (.), (<>))

builder2bs :: Builder -> ByteString
builder2bs = toStrict . BB.toLazyByteString

-- * Lexing utility

-- | Cons for 'Builder's
(<:>) :: Char -> Builder -> Builder
(<:>) = \c -> (BB.char8 c <>)

bstakewhile :: Parser Char -> Parser ByteString
bstakewhile p = byteStringOf $ skipMany $ p

bstakewhile1 :: Parser Char -> Parser ByteString
bstakewhile1 p = byteStringOf $ skipSome $ p

-- | Parse @a@ but also make sure @q@ cannot parse (must consume span).
butnot :: Parser a -> Parser b -> Parser a
butnot p q = withSpan p \x sp -> ((inSpan sp q >> eof) `fails`) $> x
