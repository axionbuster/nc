module Language.NC.ParseDec where

import Language.NC.CTypes
import Language.NC.Expr
import Language.NC.Internal.Prelude
import Language.NC.THLex

-- We do stateless parsing here (here lexing and parsing are combined).

-- * Lexing

-- | Match an identifier
ident :: Parser Span
ident = spanOf (mainparse `butnot` anykeyword)
  where
    mainparse = ident_h >> ident_t
    ident_h = skipSatisfy \c -> isAlpha c || c == '_'
    ident_t = (skipSatisfy \c -> isAlphaNum c || c == '_') `mplus` ident_t

-- | A user-definable operator character predicate
opchar_b :: Char -> Bool
opchar_b = (`elem` "!#$%&*+./<=>?@\\^|-~:")

-- | Extension: a user-definable operator character
opchar :: Parser ()
opchar = skipSatisfy opchar_b

-- | Extension: a user-defineable operator sequence
operator :: Parser Span
operator = spanOf (skipSome opchar)

-- | Parse a \"primitive\", non-derived type, and its span
primtype :: Parser (WithSpan PrimType)
primtype = lexeme $ withSpan _primtype pwithspan
