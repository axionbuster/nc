{-# OPTIONS_GHC -Wno-name-shadowing -Wno-missing-signatures #-}

-- | A new lexing module that's intended to closely align with the official
-- lexical grammar of \"C99\" as described in the N1256 technical report.
--
-- (Well, the report does **NOT** describe C99 proper. But it's free!)
--
-- Most actions either just succeed or fail without emitting a token.
-- Sometimes, it will return a useful value on success.
--
-- Also, it does NOT automatically handle whitespace and comments. Each lexeme
-- should be wrapped in a 'lexeme' function.
module Language.NC.Lex2
  ( -- * Lexing (6.4 Lexical elements)
    token,
    preprocessing_token,

    -- ** 6.4.1 Keywords
    keyword,

    -- ** 6.4.2 Identifiers
    identifier,
    universal_character_name,

    -- ** 6.4.4 Constants
    constant,
    integer_constant,
    character_constant,
    enumeration_constant,
    floating_constant,

    -- ** 6.4.5 String literals
    string_literal,

    -- ** 6.4.6 Punctuators
    punctuator,

    -- ** 6.4.7 Header names
    header_name,

    -- ** 6.4.8 Preprocessing numbers
    pp_number,

    -- * Numerals

    -- - Regular: recognize digit
    -- - One prime: retrieve digit
    -- - Two primes: convert to number
    digit,
    digit',
    digit'',
    hexdigit,
    hexdigit',
    hexdigit'',
    octdigit,
    octdigit',
    octdigit'',

    -- * Lexeme
    lexeme,

    -- * Data structures
    Constexpr99 (..),

    -- * Comments and whitespace
    ws,
    ws1,

    -- * Utility
    switch',
    switch1',
  )
where

import Language.Haskell.TH.Syntax
import Language.NC.CTypes qualified as C
import Language.NC.Internal.Prelude

-- PERF NOTE: backtracking can be expensive. Use the most precise and
-- specific rule you can to avoid backtracking.

-- helper

_satasc = skipSatisfyAscii

-- 6.4 Lexical elements.

token = choice [keyword, identifier, void constant, string_literal, punctuator]

preprocessing_token =
  choice
    [ header_name,
      identifier,
      pp_number,
      void character_constant,
      string_literal,
      punctuator
      -- also includes "each non-white-space character that
      -- cannot be one of the above"
    ]

-- 6.4.1 Keywords

keyword =
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
         |]
   )

-- 6.4.2 Identifiers

-- 6.4.2.1 General

identifier =
  choice
    [ identifier_nondigit,
      identifier >> identifier_nondigit,
      identifier >> digit
    ]

identifier_nondigit = choice [nondigit, universal_character_name]

nondigit = _satasc \c -> isLatinLetter c || c == '_'

digit = _satasc isDigit

digit' = satisfyAscii isDigit

digit'' = digit' <&> \c -> ord c - ord '0'

-- 6.4.3 Universal character names

universal_character_name =
  $( switch
       [|
         case _ of
           "\\u" -> h >> h >> h >> h
           "\\U" -> h >> h >> h >> h >> h >> h >> h >> h
         |]
   )
  where
    h = _satasc isHexDigit

-- 6.4.4 Constants

-- | Assemble a number from a base, parser, and max number of digits.
--
-- Set max number to a negative number to consume all digits.
--
-- Require at least one digit be consumed, or else fail.
asm b p = go 1 0
  where
    go r n = \case
      (0 :: Int) -> pure n
      i -> do
        d <- p
        go (r * b) (n + d * r) (i - 1) <|> pure n

-- | A C99 constant expression
data Constexpr99
  = -- | Integer or character constant. Character constants have the type
    -- of @int@ and are represented as such.
    IntConst Integer C.PrimType
  | -- | Floating point constant.
    FloatConst Double C.PrimType
  | -- | Enumeration constant, having @int@ type. However,
    -- value cannot be ascertained until later, so we return
    -- a 'Span' to the identifier, which can be looked up
    -- in the symbol table.
    EnumConst Span
  | -- | String literal's span.
    StringConst Span
  deriving (Eq, Show)

hexdigit = _satasc isHexDigit

hexdigit' = satisfyAscii isHexDigit

hexdigit'' = hexdigit' <&> \c -> ord c - ord '0'

constant =
  choice
    [ integer_constant,
      floating_constant,
      character_constant,
      enumeration_constant
    ]

-- FIXME: now it will parse but if you try to use it, it will diverge
-- in the parser with a TODO error
floating_constant =
  $( switch
       [|
         case _ of
           "0x" -> hex
           "0X" -> hex
           _ -> dec
         |]
   )
    >> sfx
    <&> FloatConst (error "floating point value not implemented")
  where
    digits = skipSome digit
    hexdigits = skipSome hexdigit
    hex =
      branch $(char '.') hexdigits (skipMany hexdigit >> optional_ $(char '.'))
        >> bexpo
    sign = _satasc \c -> c == '+' || c == '-'
    bexpo =
      _satasc (\c -> c == 'p' || c == 'P')
        >> optional_ sign
        >> digits
    expo =
      _satasc (\c -> c == 'e' || c == 'E')
        >> optional_ sign
        >> digits
    dec =
      branch
        $(char '.')
        (digits >> optional_ expo)
        ( digits
            >> branch
              $(char '.')
              (skipMany digit >> optional_ expo)
              expo -- mandatory now
        )
    sfx =
      $( switch
           [|
             case _ of
               "f" -> pure C.Float_
               "F" -> pure C.Float_
               "l" -> pure C.LongDouble_
               "L" -> pure C.LongDouble_
               _ -> pure C.Double_
             |]
       )

octdigit = _satasc isOctDigit

octdigit' = satisfyAscii isOctDigit

octdigit'' = octdigit' <&> \c -> ord c - ord '0'

-- rule was small enough to "compile" by hand
integer_constant =
  $( switch
       [|
         case _ of
           "0x" -> hex
           "0" -> oct
           _ -> dec
         |]
   )
    >>= \n -> sfx <&> IntConst (fromIntegral n)
  where
    -- start with integer type
    u = pure C.UInt_
    i = pure C.Int_
    l = pure C.Long_
    ll = pure C.LongLong_
    ul = pure C.UInt_
    ull = pure C.ULongLong_
    -- main integer parsing
    upd b p = asm b p (-1)
    hex = upd 16 hexdigit''
    oct = upd 8 octdigit''
    dec = upd 10 digit''
    sfx =
      $( switch
           [|
             case _ of
               "ul" -> ul
               "uL" -> ul
               "ull" -> ull
               "uLL" -> ull
               "Ul" -> ul
               "UL" -> ul
               "Ull" -> ull
               "ULL" -> ull
               "lu" -> ul
               "Lu" -> ul
               "llu" -> ull
               "LLu" -> ull
               "lU" -> ull
               "LU" -> ull
               "llU" -> ull
               "LLU" -> ull
               "u" -> u
               "U" -> u
               "l" -> l
               "L" -> l
               "ll" -> ll
               "LL" -> ll
               _ -> i
             |]
       )

-- | NOTE: this produces a C @int@ constant, NOT @char@ constant.
character_constant =
  optional_ $(char 'L') -- multi-byte. we ignore this for now.
    >> between $(char '\'') $(char '\'') cchar
  where
    cchar =
      $( switch
           [|
             case _ of
               "\\" -> esc
               "\'" -> esc
               "\n" -> esc
               "\r" -> esc
               -- carriage return being 2 chars and having to
               -- use maximum munch, super annoying to reach out to
               -- 'switch' here
               "\r\n" -> esc
               _ -> anyChar <&> ord
             |]
       )
        <&> \i -> IntConst (fromIntegral i) C.Int_
    esc =
      $( switch
           [|
             case _ of
               "\\'" -> pure (ord '\'')
               "\\\"" -> pure (ord '"')
               "\\\\" -> pure (ord '\\')
               "\\n" -> pure (ord '\n')
               "\\t" -> pure (ord '\t')
               "\\r" -> pure (ord '\r')
               "\\a" -> pure (ord '\a')
               "\\b" -> pure (ord '\b')
               "\\f" -> pure (ord '\f')
               "\\v" -> pure (ord '\v')
               "\\?" -> pure (ord '\DEL')
               "\\x" -> hex -- note. \X is missing
               "\\u" -> uni
               "\\U" -> uni2
               "\\" -> oct
             |]
       )
      where
        -- base, parser, max number of digits
        uni2 = asm 16 hexdigit'' 8
        hex = asm 16 hexdigit'' 8
        uni = asm 16 hexdigit'' 4
        oct = asm 8 octdigit'' 12

enumeration_constant = spanOf identifier <&> EnumConst

-- 6.4.5 String literals

string_literal = internal
  where
    internal = optional_ $(char 'L') >> between $(char '"') $(char '"') schars
    schars = skipMany schar
    schar =
      $( switch
           [|
             case _ of
               "\\" -> esc
               "\"" -> esc
               "\n" -> esc
               "\r" -> esc
               "\r\n" -> esc
               _ -> skipAnyChar
             |]
       )
    esc =
      $( switch
           [|
             case _ of
               "\\'" -> pure ()
               "\\\"" -> pure ()
               "\\\\" -> pure ()
               "\\n" -> pure ()
               "\\t" -> pure ()
               "\\r" -> pure ()
               "\\a" -> pure ()
               "\\b" -> pure ()
               "\\f" -> pure ()
               "\\v" -> pure ()
               "\\?" -> pure ()
               "\\x" -> upto 2 hexdigit
               "\\" -> upto 3 octdigit
               "\\u" -> uni
               "\\U" -> uni
             |]
       )
    uni = skipBack 2 >> universal_character_name
    upto 0 _ = pure ()
    upto n p = p >> optional_ (upto (n - (1 :: Int)) p)

-- * 6.4.6 Punctuators

-- NOTE: With custom operators coming in "New C," maybe we should rethink
-- the definition of punctuator.
punctuator =
  $( switch
       [|
         case _ of
           "[" -> pure ()
           "]" -> pure ()
           "(" -> pure ()
           ")" -> pure ()
           "{" -> pure ()
           "}" -> pure ()
           "." -> pure ()
           "->" -> pure ()
           "++" -> pure ()
           "--" -> pure ()
           "&" -> pure ()
           "*" -> pure ()
           "+" -> pure ()
           "-" -> pure ()
           "~" -> pure ()
           "!" -> pure ()
           "/" -> pure ()
           "%" -> pure ()
           "<<" -> pure ()
           ">>" -> pure ()
           "<" -> pure ()
           ">" -> pure ()
           "<=" -> pure ()
           ">=" -> pure ()
           "==" -> pure ()
           "!=" -> pure ()
           "^" -> pure ()
           "|" -> pure ()
           "&&" -> pure ()
           "||" -> pure ()
           "?" -> pure ()
           ":" -> pure ()
           ";" -> pure ()
           "..." -> pure ()
           "=" -> pure ()
           "*=" -> pure ()
           "/=" -> pure ()
           "%=" -> pure ()
           "+=" -> pure ()
           "-=" -> pure ()
           "<<=" -> pure ()
           ">>=" -> pure ()
           "&=" -> pure ()
           "^=" -> pure ()
           "|=" -> pure ()
           "," -> pure ()
           "#" -> pure ()
           "##" -> pure ()
           "<:" -> pure ()
           ":>" -> pure ()
           "<%" -> pure ()
           "%>" -> pure ()
           "%:" -> pure ()
           "%:%:" -> pure ()
         |]
   )

-- * 6.4.7 Header names

header_name = $(switch [|case _ of "<" -> an; "\"" -> qu|])
  where
    an = skipMany $ skipSatisfy (/= '>')
    qu = skipMany $ skipSatisfy (/= '"')

-- * 6.4.8 Preprocessing numbers

{-
Original left-recursive grammar was converted to right-recursive
grammar so it could be dealt with by the parser. This isn't the
only strategy that works, but it is the one I chose.

ORIGINAL

pp-number:
  digit
  . digit
  pp-number digit
  pp-number identifier-nondigit
  pp-number e sign
  pp-number E sign
  pp-number p sign
  pp-number P sign
  pp-number .

NEW

pp-number:
  digit pp-number'
  . digit pp-number'

pp-number':
  digit pp-number'
  identifier-nondigit pp-number'
  e sign pp-number'
  E sign pp-number'
  p sign pp-number'
  P sign pp-number'
  . pp-number'
  ε
-}

pp_number = optional_ $(char '.') >> digit >> go
  where
    sign = _satasc \c -> c == '+' || c == '-'
    go =
      (digit >> go)
        <|> (identifier_nondigit >> go)
        <|> $( switch
                 [|
                   case _ of
                     "e" -> sign >> go
                     "E" -> sign >> go
                     "p" -> sign >> go
                     "P" -> sign >> go
                     "." -> go
                     _ -> pure ()
                   |]
             )

-- * Comments and whitespace

-- | Consume line. Windows, Classic MacOS, and UNIX line endings.
consumeline =
  $( switch
       [|
         case _ of
           "\r" -> ws
           "\r\n" -> ws
           "\n" -> ws
           _ -> branch skipAnyChar consumeline eof
         |]
   )

-- | Find end of block comment (@*\/@)
findendcomment =
  $( switch
       [|
         case _ of
           "*/" -> ws
           _ -> skipAnyChar >> findendcomment
         |]
   )

-- | Consume whitespace and comments, reading up to 4 characters at a time.
ws :: Parser ()
ws =
  $( switch
       [|
         case _ of
           "    " -> ws
           "  " -> ws
           " " -> ws
           "\t\t\t\t" -> ws
           "\t\t" -> ws
           "\t" -> ws
           "\n" -> ws
           "\r" -> ws
           "\v" -> ws
           "\f" -> ws
           "//" -> consumeline
           "/*" -> findendcomment
           _ -> pure ()
         |]
   )

-- | Consume some whitespace and comments. Something is required.
-- This is used for lexing identifiers.
ws1 :: Parser ()
ws1 =
  $( switch
       [|
         case _ of
           "    " -> ws
           "  " -> ws
           " " -> ws
           "\t\t\t\t" -> ws
           "\t\t" -> ws
           "\t" -> ws
           "\n" -> ws
           "\r" -> ws
           "\v" -> ws
           "\f" -> ws
           "//" -> consumeline
           "/*" -> findendcomment
           _ -> eof
         |]
   )

-- * Some parser specific stuff

-- | like 'switch', but after each match, consume any (optional) whitespace
switch' :: Q Exp -> Q Exp
switch' = switchWithPost (Just [|ws|])

-- | like 'switch', but after each match, consume some whitespace
switch1' :: Q Exp -> Q Exp
switch1' = switchWithPost (Just [|ws1|])

-- | Parse a thing and then require whitespace or end of file, which is
-- also consumed. If there's an error, annotate it with a span, log it, and
-- then rethrow the error. Use 'try' to suppress the error.
lexeme :: Parser a -> Parser a
lexeme p = do
  st <- getPos
  let apologize e = do
        en <- getPos
        es <- pserrors <$> ask
        modifyIORef es (:|> aenew e (Span st en))
        err e
  withError p apologize <* ws1
