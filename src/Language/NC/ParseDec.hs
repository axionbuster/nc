module Language.NC.ParseDec where

import Data.ByteString.Builder qualified as BB
import Data.ByteString.Char8 qualified as C
import Language.NC.Prelude
import Language.NC.THLex

-- in this temporarily small module, we'll be lexing and parsing together.

-- * Lexing

-- honestly, doing this for ByteString parsing is kind of tedious.
-- there has to be a better way ... for less-hot places, probably
-- ok to use regular strings which are much nicer to work with.

-- | Match an identifier
ident_bs :: Parser ByteString
ident_bs = mainparse `butnot` anykeyword
  where
    mainparse = C.cons <$> ident_h <*> ident_t
    ident_h = satisfy \c -> isAlpha c || c == '_'
    ident_t = bstakewhile1 $ satisfy \c -> isAlphaNum c || c == '_'

-- | Match an identifier
ident :: Parser Builder
ident = BB.byteString <$> ident_bs

-- | Extension: a user-definable operator character
opchar :: Parser Char
opchar = satisfy (`elem` "!#$%&*+./<=>?@\\^|-~:")

-- | Extension: a user-defineable operator sequence
operator :: Parser Builder
operator = ((<:>) <$> opchar <*> operator) <|> pure mempty
