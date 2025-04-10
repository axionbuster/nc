{-# OPTIONS_GHC -Wno-name-shadowing -Wno-missing-signatures #-}

-- | Lexers and basic parsers.
--
-- Lexing convention:
--  - Each parser\/lexer assumes the respective token begins directly rather
--    than potentially starting with whitespace. Consequently, these primitives
--    do not begin with whitespace parsing.
--  - Keywords and punctuation will consume any whitespace, EOF,
--    punctuation, or comments---collectively refered to as \'space\'. This
--    includes in-delimiter parsers like 'inpar'.
--  - Keywords and identifiers will require some space to come afterward.
module Language.NC.Internal.Lex (
  -- * Whitespace and comments
  ws0,
  ws1,
  lx0,
  lx1,
  switch_ws0,
  switch_ws1,

  -- * Punctuation
  punct,
  ldbsqb,
  rdbsqb,
  lsqb,
  rsqb,
  lpar,
  rpar,
  lcur,
  rcur,
  branch_between,
  insqb,
  branch_insqb,
  indbsqb,
  branch_indbsqb,
  inpar,
  branch_inpar,
  incur,
  branch_incur,
  comma,
  semicolon,
  quote,
  dbquote,
  period,
  rarrow,
  dbplus,
  dbminus,
  ampersand,
  caret,
  bar,
  tilde,
  plus,
  minus,
  star,
  slash,
  percent,
  starslash,
  less,
  greater,
  lessequal,
  greaterequal,
  dbequal,
  notequal,
  bang,
  dbamp,
  dbbar,
  equal,
  starequal,
  slashequal,
  percentequal,
  plusequal,
  minusequal,
  ampersandequal,
  caretequal,
  barequal,
  tildeequal,
  dblessequal,
  dbgreaterequal,
  colon,
  dbcolon,
  questionmark,
  hash',
  dbhash',
  backslash,
  tripledot,

  -- * Keywords
  keyword,
  alignas',
  alignof',
  auto',
  break',
  case',
  char',
  const',
  constexpr',
  continue',
  default',
  do',
  double',
  else',
  enum',
  extern',
  false',
  float',
  for',
  goto',
  if',
  inline',
  int',
  long',
  nullptr',
  nullptr_t',
  register',
  restrict',
  return',
  short',
  signed',
  sizeof',
  static',
  static_assert',
  struct',
  switch',
  thread_local',
  true',
  typedef',
  typeof',
  typeof_unequal',
  union',
  unsigned',
  void',
  volatile',
  while',
  _Atomic',
  _BitInt',
  _Bool',
  _Complex',
  _Decimal128',
  _Decimal32',
  _Decimal64',
  _Generic',
  _Imaginary',
  _Noreturn',

  -- * Reserved or special identifiers
  __func__',
  main',
  memset',
  printf',
  bs_dbunderscore,
  bs_hasdbunderscore,

  -- * Identifiers
  identifier,
  identifier_def,

  -- * Universal character names
  universal_character_name,
  ucnam_val_word32,

  -- * Literals
  literal,
  integer_constant,
  integer_constant_val,
  floating_constant,
  character_constant,
  character_constant_val,
  string_literal,
  string_literal_val,

  -- * Higher-order constructs
  butnot,
  butnotpfx,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Char8 qualified as C8
import Data.Text.ICU.Char qualified as C
import GHC.Int (Int (..))
import GHC.Integer.Logarithms (integerLog2#)
import Language.NC.Internal.Prelude

literal =
  branch $(char '\'') (skipBack 1 >> LitChar <$> character_constant_val)
    $ branch _decdigit (skipBack 1 >> LitInteger <$> integer_constant_val)
    $ (LitString <$> string_literal_val) -- formatter cries if no parens here

-- * Whitespace and comments

-- first, smart punctuator detection for boundary detection...

-- only the first character of a punctuator is needed to detect a
-- boundary. also includes whitespace and comments, which are not
-- punctuators, but are needed for boundary detection.
punct = skipSatisfyAscii (`elem` "!%&()*+,-./:;<=>?[]\\^{|}~")

-- the reason why we take 'punct' into account here is that when we
-- say "whitespace is required," what we almost certainly mean is that
-- identifiers should not join together. so ... punctuators are also
-- included in the set of things that can separate identifiers. that's why.

ws_base nomat =
  $( switch
       [|
         case _ of
           "    " -> ws0
           "  " -> ws0
           " " -> ws0
           "//" -> consumeline
           "/*" -> findendcomment
           "\a" -> ws0
           "\b" -> ws0
           "\f" -> ws0
           "\n" -> ws0
           "\r" -> ws0
           "\t" -> ws0
           "\v" -> ws0
           _ -> lookahead punct <|> nomat
         |]
   )
{-# INLINE ws_base #-}

-- | Consume line. Windows, Classic MacOS, and UNIX line endings.
consumeline =
  $( switch
       [|
         case _ of
           "\r" -> ws0
           "\r\n" -> ws0
           "\n" -> ws0
           _ -> branch skipAnyChar consumeline eof
         |]
   )

-- | Find end of block comment (@*\/@)
findendcomment =
  $( switch
       [|
         case _ of
           "*/" -> ws0
           _ -> skipAnyChar >> findendcomment
         |]
   )

-- | Some or no whitespace or comments or punctuators
ws0 = ws_base (pure ())

-- | At least some whitespace or comments or punctuators
ws1 = ws_base eof

-- * Operators, keywords, and identifiers

-- | @[@
_lsq = lx0 $(switch [|case _ of "[" -> pure (); "<:" -> pure ()|])

-- | @]@
_rsq = lx0 $(switch [|case _ of "]" -> pure (); ":>" -> pure ()|])

-- | @[[@ and @]]@, respectively.
(ldbsqb, rdbsqb) =
  -- The C standard allows whitespace to go between them.
  (_lsq >> _lsq, _rsq >> _rsq)

-- | These check that they're not followed by another copy of the respective
-- bracket (e.g., @[[@, @]]@).
(lsqb, rsqb) = (_lsq `notFollowedBy` _lsq, _rsq `notFollowedBy` _rsq)

(lpar, rpar) = (lx0 $(char '('), lx0 $(char ')'))

(lcur, rcur) = (lx0 opening, lx0 closing)
 where
  opening = $(switch [|case _ of "{" -> pure (); "<%" -> pure ()|])
  closing = $(switch [|case _ of "}" -> pure (); "%>" -> pure ()|])

-- | Parse o. If ok, parse p and then parse c and return what p returns.
-- If o doesn't parse, then parse q. Usage: @branch_between o c p q@
branch_between ::
  ParserT st r e open ->
  ParserT st r e close ->
  ParserT st r e a ->
  ParserT st r e a ->
  ParserT st r e a
branch_between o c p = branch o (p <* c)

-- | Enclose a thing in square brackets not followed by another square bracket.
insqb = between lsqb rsqb

-- | Parse the first parser in square brackets if opening (left) square bracket
-- is detected. Otherwise, parse q. This cuts down on needless backtracking.
branch_insqb = branch_between lsqb rsqb

-- | Enclose a thing in double square brackets.
indbsqb = between ldbsqb rdbsqb

-- | Parse the first parser in double square brackets if opening double
-- square bracket is detected. Otherwise, parse q. This cuts down on needless
-- backtracking.
branch_indbsqb = branch_between ldbsqb rdbsqb

-- | Enclose a thing in parentheses.
inpar = between lpar rpar

-- | Parse the first parser in parentheses if opening parenthesis
-- is detected. Otherwise, parse q. This cuts down on needless
-- backtracking.
branch_inpar = branch_between lpar rpar

-- | Enclose a thing in braces.
incur = between lcur rcur

-- | Parse the first parser in (curly) braces if opening brace
-- is detected. Otherwise, parse q. This cuts down on needless
-- backtracking.
branch_incur = branch_between lcur rcur

-- Important punctuations

(comma, semicolon) = (lx0 $(char ','), lx0 $(char ';'))

(quote, dbquote) = (lx0 $(char '\''), lx0 $(char '"'))

-- Member access operators
(period, rarrow) = (lx0 $(char '.'), lx0 $(string "->"))

-- Increment/decrement operators
(dbplus, dbminus) = (lx0 $(string "++"), lx0 $(string "--"))

-- Bitwise operators
(ampersand, caret, bar, tilde) =
  ( lx0 $(char '&'),
    lx0 $(char '^'),
    lx0 $(char '|'),
    lx0 $(char '~')
  )

-- Basic arithmetic operators
(plus, minus, star, slash, percent) =
  (lx0 $(char '+'), lx0 $(char '-'), lx0 $(char '*'), lx0 s, lx0 $(char '%'))
 where
  -- most operators do "clash" (share a prefix with another operator)
  -- but the division operator clashes with comments so we will
  -- check for that specifically
  s =
    $( switch
         [|
           case _ of
             "//" -> failed
             "/*" -> failed
             "/" -> pure ()
           |]
     )

-- this isn't an *operator* but sometimes it appears because a block
-- comment wasn't closed properly

starslash = lx0 $(string "*/")

-- Relational operators
(less, greater) = (lx0 $(char '<'), lx0 $(char '>'))

(lessequal, greaterequal) = (lx0 $(string "<="), lx0 $(string ">="))

(dbequal, notequal) = (lx0 $(string "=="), lx0 $(string "!="))

-- Logical operators
(bang, dbamp, dbbar) =
  ( lx0 $(char '!'),
    lx0 $(string "&&"),
    lx0 $(string "||")
  )

-- Assignment operator
equal = lx0 $(char '=')

-- Compound assignments
(starequal, slashequal, percentequal, plusequal, minusequal) =
  ( lx0 $(string "*="),
    lx0 $(string "/="),
    lx0 $(string "%="),
    lx0 $(string "+="),
    lx0 $(string "-=")
  )

(ampersandequal, caretequal, barequal, tildeequal) =
  ( lx0 $(string "&="),
    lx0 $(string "^="),
    lx0 $(string "|="),
    lx0 $(string "~=")
  )

(dblessequal, dbgreaterequal) =
  ( lx0 $(string "<<="),
    lx0 $(string ">>=")
  )

-- ** Misc operators (and parts of operators)

(colon, dbcolon) = (lx0 $(char ':'), lx0 $(string "::"))

questionmark = lx0 $(char '?')

-- ** Preprocessor 'operators'

(hash', dbhash') =
  ( lx0 $(switch [|case _ of "#" -> pure (); "%:" -> pure ()|]),
    lx0 $(switch [|case _ of "##" -> pure (); "%:%:" -> pure ()|])
  )

backslash = lx0 $(char '\\')

-- ** Ellipsis

tripledot = lx0 $(string "...")

-- ** Keywords

-- Same warning applies as above; they aren't defined as lexemes (yet)
-- so it will be insensitive to whitespace, lookahead is not handled, etc.
-- Basically no semantic grouping is done here.

alignas' =
  lx1
    $(switch [|case _ of "alignas" -> pure (); "_Alignas" -> pure ()|])

alignof' =
  lx1
    $( switch
         [|
           case _ of
             "alignof" -> pure ()
             "_Alignof" -> pure ()
           |]
     )

auto' = lx1 $(string "auto")

break' = lx1 $(string "break")

case' = lx1 $(string "case")

char' = lx1 $(string "char")

const' = lx1 $(string "const")

constexpr' = lx1 $(string "constexpr")

continue' = lx1 $(string "continue")

default' = lx1 $(string "default")

do' = lx1 $(string "do")

double' = lx1 $(string "double")

else' = lx1 $(string "else")

enum' = lx1 $(string "enum")

extern' = lx1 $(string "extern")

false' = lx1 $(string "false")

float' = lx1 $(string "float")

for' = lx1 $(string "for")

goto' = lx1 $(string "goto")

if' = lx1 $(string "if")

inline' = lx1 $(string "inline")

int' = lx1 $(string "int")

long' = lx1 $(string "long")

nullptr' = lx1 $(string "nullptr")

nullptr_t' = lx1 $(string "nullptr_t")

register' = lx1 $(string "register")

restrict' = lx1 $(string "restrict")

return' = lx1 $(string "return")

short' = lx1 $(string "short")

signed' = lx1 $(string "signed")

sizeof' = lx1 $(string "sizeof")

static' = lx1 $(string "static")

static_assert' =
  lx1
    $( switch
         [|
           case _ of
             "static_assert" -> pure ()
             "_Static_assert" -> pure ()
           |]
     )

struct' = lx1 $(string "struct")

switch' = lx1 $(string "switch")

thread_local' =
  lx1
    $( switch
         [|
           case _ of
             "thread_local" -> pure ()
             "_Thread_local" -> pure ()
           |]
     )

true' = lx1 $(string "true")

typedef' = lx1 $(string "typedef")

typeof' = lx1 $(string "typeof")

typeof_unequal' = lx1 $(string "typeof_unequal")

union' = lx1 $(string "union")

unsigned' = lx1 $(string "unsigned")

void' = lx1 $(string "void")

volatile' = lx1 $(string "volatile")

while' = lx1 $(string "while")

_Atomic' = lx1 $(string "_Atomic")

_BitInt' = lx1 $(string "_BitInt")

_Bool' = lx1 $(string "_Bool")

_Complex' = lx1 $(string "_Complex")

_Decimal128' = lx1 $(string "_Decimal128")

_Decimal32' = lx1 $(string "_Decimal32")

_Decimal64' = lx1 $(string "_Decimal64")

_Generic' = lx1 $(string "_Generic")

_Imaginary' = lx1 $(string "_Imaginary")

_Noreturn' = lx1 $(string "_Noreturn")

-- ** some reserved identifiers.

-- user code may not declare these identifiers. they have special
-- meaning in the C standard. however, these are not keywords.

__func__' = lx1 $(string "__func__")

-- ** Any notable identifiers.

-- this is an ever expanding list of identifiers that are not keywords
-- or reserved identifiers, but are notable in some way.

main' = lx1 $(string "main")

memset' = lx1 $(string "memset")

printf' = lx1 $(string "printf")

-- ** Helping find 'bad' syntax for identifiers.

-- They are allowed by the standard, but are not recommended except
-- in special circumstances (e.g., standard library implementations):
--
--  - Identifier begins with an underscore followed
--    by an uppercase Latin letter.
--  - Identifier contains two consecutive underscores anywhere.

bs_dbunderscore = BS.pack [95, 95]

bs_hasdbunderscore = (bs_dbunderscore `BS.isInfixOf`)

-- * Lexing (6.4 Lexical elements)

-- ** Keywords

-- | C23 keywords
keyword =
  lx1
    $( switch
         [|
           case _ of
             "alignas" -> pure ()
             "alignof" -> pure ()
             "auto" -> pure ()
             "break" -> pure ()
             "case" -> pure ()
             "char" -> pure ()
             "const" -> pure ()
             "constexpr" -> pure ()
             "continue" -> pure ()
             "default" -> pure ()
             "do" -> pure ()
             "double" -> pure ()
             "else" -> pure ()
             "enum" -> pure ()
             "extern" -> pure ()
             "false" -> pure ()
             "float" -> pure ()
             "for" -> pure ()
             "goto" -> pure ()
             "if" -> pure ()
             "inline" -> pure ()
             "int" -> pure ()
             "long" -> pure ()
             "nullptr" -> pure ()
             "nullptr_t" -> pure ()
             "register" -> pure ()
             "restrict" -> pure ()
             "return" -> pure ()
             "short" -> pure ()
             "signed" -> pure ()
             "sizeof" -> pure ()
             "static" -> pure ()
             "static_assert" -> pure ()
             "struct" -> pure ()
             "switch" -> pure ()
             "thread_local" -> pure ()
             "true" -> pure ()
             "typedef" -> pure ()
             "typeof" -> pure ()
             "typeof_unqual" -> pure ()
             "union" -> pure ()
             "unsigned" -> pure ()
             "void" -> pure ()
             "volatile" -> pure ()
             "while" -> pure ()
             "_Alignas" -> pure ()
             "_Alignof" -> pure ()
             "_Atomic" -> pure ()
             "_BitInt" -> pure ()
             "_Bool" -> pure ()
             "_Complex" -> pure ()
             "_Decimal128" -> pure ()
             "_Decimal32" -> pure ()
             "_Decimal64" -> pure ()
             "_Generic" -> pure ()
             "_Imaginary" -> pure ()
             "_Noreturn" -> pure ()
             "_Static_assert" -> pure ()
             "_Thread_local" -> pure ()
           |]
     )

-- ** 6.4.2 Identifiers

-- *** 6.4.2.1 General

-- optimized somewhat

-- | Any identifier. This allows reserved identifiers like @__func__@
-- to be used. For use in declarations, consider 'identifier_def' instead.
identifier = lx1 $ byteStringOf $ (id_head >> skipMany id_tail) `butnot` keyword
 where
  id_head = branch slash universal_character_name nondigit
  id_tail = branch slash universal_character_name fullset
  nondigit = skipFusedSatisfy asciind xids xids xids
  fullset = skipFusedSatisfy asciifull xidc xidc xidc
  asciind = \c -> isLatinLetter c || c == '_'
  asciifull = \c -> isDigit c || isLatinLetter c || c == '_'
  xids = C.property C.XidStart
  xidc = C.property C.XidContinue

-- | A user-definable identifier. Currently, only @__func__@ is banned.
identifier_def = do
  i <- identifier
  guard (i /= C8.pack "__func__") $> i

-- *** 6.4.3 Universal character names

-- | Recognize a universal character name character.
--
-- The value-returning counterpart is called 'ucnam_val_word32'.
universal_character_name = () <$ ucnam_val_word32

-- | A Unicode character (in a 'Word32')
ucnam_val_word32 =
  $( switch
       [|
         case _ of
           "\\u" -> fromIntegral @_ @Word32 <$> isolate 4 anyAsciiHexWord
           "\\U" -> fromIntegral @_ @Word32 <$> isolate 8 anyAsciiHexWord
         |]
   )
    >>= \c ->
      c <$ guard do
        (c < 0x00A0 && c `notElem` map (fromIntegral . ord) "$@`")
          || (0xD800 <= c && c <= 0xDFFF)
          || (c > 0x10FFFF)

-- *** 6.4.4 Constants

-- | Assemble a number from a base, parser, digit separator,
-- and an optional max number of digits.
--
-- The third argument is a parser for the digit separator. Give it @'pure' ()@
-- to disable it.
--
-- Set max number to a negative number to consume all digits.
--
-- Require at least one digit be consumed, or else fail.
asm b p sep = go 0 True
 where
  -- making sure separators can't appear in a row.
  go n sepfresh = \case
    (0 :: Int) -> pure n
    i ->
      branch
        (sep >> guard sepfresh)
        (go n False i)
        ( withOption
            p
            (\d -> go (n * b + d) True (i - 1))
            (pure n)
        )

_hexdigit =
  satisfyAscii isHexDigit <&> fromIntegral @_ @Integer . \c ->
    if
      | 'A' <= c && c <= 'F' -> ord c - ord 'A' + 10
      | 'a' <= c && c <= 'f' -> ord c - ord 'a' + 10
      | '0' <= c && c <= '9' -> ord c - ord '0'
      | otherwise -> error "_hexdigit: impossible"

_octdigit =
  satisfyAscii isOctDigit <&> fromIntegral @_ @Integer . \c -> ord c - ord '0'

_decdigit =
  satisfyAscii isDigit <&> fromIntegral @_ @Integer . \c -> ord c - ord '0'

_bindigit =
  $( switch
       [|
         case _ of
           "0" -> pure 0
           "1" -> pure 1
         |]
   )

-- | occurrence counts of integer literal suffices
data ISFX_
  = ISFX_ !Word8 !Word8 !Word8

instance Semigroup ISFX_ where
  ISFX_ a b c <> ISFX_ x y z = ISFX_ (a ^^+ x) (b ^^+ y) (c ^^+ z)
   where
    -- carry with overflow guard
    p ^^+ 0 = p
    255 ^^+ 1 = 255
    p ^^+ 1 = p + 1
    _ ^^+ _ = error "ISFX_'s ^^+: impossible"

instance Monoid ISFX_ where
  mempty = ISFX_ 0 0 0

-- find integral type that fits literal or else throw error.
isfx_2inttyp wasdecimal = gated_go . integerneededbits
 where
  i_ = Int_
  l_ = Long_
  ll_ = LongLong_
  u_ = UInt_
  ul_ = ULong_
  ull_ = ULongLong_
  -- find: find first type that can contain the literal
  find bw = go2
   where
    go2 [] = err $ LiteralBadError LiteralTooLarge
    go2 (t : ts) = do
      tw <- ist_preciseposbw t
      if tw >= bw
        then pure t
        else go2 ts
  gated_go bw
    | bw > fromIntegral (maxBound :: Word8) =
        const $ err $ LiteralBadError LiteralTooLarge
    | otherwise = go (fromIntegral bw)
  -- ISFX_ (u or U) (l or L) (wb or WB)
  -- Table below is found in 6.4.4.1.6 (N3088)
  go bw (ISFX_ 0 0 0)
    | wasdecimal = find bw [i_, l_, ll_]
    | otherwise = find bw [i_, u_, l_, ul_, ul_, ull_]
  go bw (ISFX_ 1 0 0) = find bw [u_, ul_, ull_]
  go bw (ISFX_ 0 1 0)
    | wasdecimal = find bw [l_, ll_]
    | otherwise = find bw [l_, ul_, ll_, ull_]
  go bw (ISFX_ 1 1 0) = find bw [ul_, ull_]
  go bw (ISFX_ 0 2 0)
    | wasdecimal = find bw [ll_]
    | otherwise = find bw [ll_, ull_]
  go bw (ISFX_ 1 2 0) = find bw [ull_]
  go bw (ISFX_ 0 0 1) = pure $ BitInt_ $ min 2 bw
  go bw (ISFX_ 1 0 1) = pure $ UBitInt_ bw
  go _ _ = err $ LiteralBadError IncorrectIntSuffix

-- used to compute bitwidth for _BitInt(N) literals.
-- as an invariant, this is >= 1.
integerneededbits 0 = 1
integerneededbits x
  | x < 0 = I# (integerLog2# (negate x)) + 1 -- sign bit
  | otherwise = I# (integerLog2# x)

-- | Parse an integer constant and the value.
--
-- Three things to note:
--  - No sign parsing: it's done by the negation (-) operator.
--  - Automatic widening as per C23: the C23 standard prescribes
--    certain automatic widenings that must occur if the literal
--    is too big to fit in the specified (if any) suffix.
--  - No guard for trailing digits: so things like \"01138\"
--    will parse just fine and return the octal number 0113 (dec 75)
--    and input 8 that's left over. that's OK; you should use it with
--    a word boundary parser like 'lx1' to reject such cases.
integer_constant_val =
  lx0
    $ $( switch
           [|
             case _ of
               "0x" -> hex
               "0X" -> hex
               "0b" -> bin
               "0B" -> bin
               "0" -> option (False, 0) oct
               _ -> dec
             |]
       )
    >>= \(wasdecimal, n) -> IntegerLiteral n <$> sfx wasdecimal n
 where
  -- the (-1) indicates max number of digits (unrestricted)
  hex = (False,) <$> asm 16 _hexdigit quote (-1)
  oct = (False,) <$> asm 8 _octdigit quote (-1)
  dec = (True,) <$> anyAsciiDecimalInteger
  bin = (False,) <$> asm 2 _bindigit quote (-1)
  sfx wasdecimal n = do
    let sfxchr =
          $( switch
               [|
                 case _ of
                   "u" -> pure $ ISFX_ 1 0 0
                   "U" -> pure $ ISFX_ 1 0 0
                   "l" -> pure $ ISFX_ 0 1 0
                   "L" -> pure $ ISFX_ 0 1 0
                   "wb" -> pure $ ISFX_ 0 0 1
                   "WB" -> pure $ ISFX_ 0 0 1
                 |]
           )
    chainl (<>) (pure mempty) sfxchr >>= isfx_2inttyp wasdecimal n

-- | Parse an integer constant; discard the value, if any.
integer_constant = () <$ integer_constant_val

oneof_ascii _set = skipSatisfyAscii (`elem` _set)

-- | Parse a floating-point literal. Discard the value.
floating_constant =
  lx0
    $
    -- TODO: move onto a parser that also produces the value.
    -- Unfortunately, handling floating points correctly for the *target*
    -- platform is a bit involved and I will need to invest more time
    -- trying to support them.
    -- THANKS: Thanks a lot to Claude (AI) for shrinking the original
    -- 2-page CFG down to like 3 PEG clauses.
    (branch hexpfx hex dec)
    >> optional_ sfx
 where
  -- floating-point suffix.
  sfx = do
    oneof_ascii "fFlLdD"
    oneof_ascii "flFL"
  exp e = do
    oneof_ascii e
    optional_ $ oneof_ascii "+-"
    skipSome $ skipSatisfyAscii isDigit
  -- "mantissa" ... which is the main part.
  -- f scans a digit.
  man f =
    (skipSome f >> optional_ (period >> skipMany f))
      <|> (period >> skipSome f)
  dec = man (skipSatisfyAscii isDigit) >> optional_ (exp "eE")
  hexpfx = $(char '0') >> oneof_ascii "xX"
  hex = do
    man $ skipSatisfyAscii isHexDigit
    -- exponent. exponent is given in decimal.
    exp "pP"

-- | Consume an integer character constant and then discard it.
character_constant = () <$ character_constant_val

char_encpfx =
  $( switch
       [|
         case _ of
           "u8" -> cst_char8_type <$> charset
           "u" -> cst_char16_type <$> charset
           "U" -> cst_char32_type <$> charset
           "L" -> cst_wchar_type <$> charset
         |]
   )
 where
  charset = _pscharset <$> ask

-- | Consume an integer character constant and then return its value.
--
-- The range is NOT checked.
character_constant_val = do
  typ <- option Int_ char_encpfx
  -- 'quote' will consume any whitespace that comes after it.
  -- it's undesirable to do that for the left single quote.
  val <- between $(char '\'') quote value
  pure $ val typ
 where
  value =
    $( switch
         [|
           case _ of
             "\\" -> esc
             "'" -> skipBack 1 >> failed
             "\n" -> failed
             "\r" -> failed
             "\r\n" -> failed
             _ -> CharacterLiteral <$> anyChar
           |]
     )
  esc = choice [simple, octal, hex, skipBack 1 >> universal]
   where
    simple = interpret <$> satisfyAscii (`elem` "'\"?\\abfnrtv")
    universal = CharacterLiteral . chr . fromIntegral <$> ucnam_val_word32
    -- FIXME: currently we assume that the host and target have the
    -- same integer endianness.
    -- base parser digit_sep (max_digits or -1 to disable)
    octal = IntCharacterLiteral <$> asm 8 _octdigit (pure ()) 3
    hex =
      IntCharacterLiteral <$> do
        $(char 'x')
        asm 16 _hexdigit (pure ()) (-1)
    interpret =
      CharacterLiteral . \case
        'a' -> '\a'
        'b' -> '\b'
        'f' -> '\f'
        'n' -> '\n'
        'r' -> '\r'
        't' -> '\t'
        'v' -> '\v'
        c -> c

-- | Parse a string literal.
string_literal_val = do
  typ <- option Char_ do
    char_encpfx
      >> err
        ( InternalError
            "string literals except regular literals not supported yet"
        )
  let enc c
        | c < 0xd800 || (0xdfff <= c && c <= 0x10ffff) = pure $ BB.char8 $ chr c
        | otherwise = err $ LiteralBadError BadChar
  -- dbquote will consume any whitespace that follows it, which is undesirable.
  val <- between $(char '"') dbquote $ chainl (<>) (pure mempty) (ch >>= enc)
  pure $ StringLiteral (BB.toLazyByteString val) typ
 where
  -- currently, no support for joining adjacent string literals, or
  -- writing a string literal across source lines using a backslash.
  ch =
    $( switch
         [|
           case _ of
             "\\" -> esc
             "\"" -> skipBack 1 >> failed
             "\n" -> failed
             "\r" -> failed
             "\r\n" -> failed
             _ -> fromIntegral . ord <$> anyChar
           |]
     )
  esc = choice [simple, octal, hex, skipBack 1 >> universal]
   where
    simple = interpret <$> satisfyAscii (`elem` "'\"?\\abfnrtv")
    universal = fromIntegral <$> ucnam_val_word32
    -- base parser digit_sep (max_digits or -1 to disable)
    octal = fromIntegral <$> asm 8 _octdigit (pure ()) 3
    hex = do $(char 'x'); fromIntegral <$> asm 16 _hexdigit (pure ()) (-1)
    interpret =
      fromIntegral . ord . \case
        'a' -> '\a'
        'b' -> '\b'
        'f' -> '\f'
        'n' -> '\n'
        'r' -> '\r'
        't' -> '\t'
        'v' -> '\v'
        c -> c

-- | Parse a string literal but discard the value.
string_literal = () <$ string_literal_val

-- | Create a Template Haskell splice like 'switch' that
-- efficiently matches strings and moves on, but also
-- optionally consumes whitespace after each match.
switch_ws0 = switchWithPost (Just [|ws0|])

-- | Create a Template Haskell splice like 'switch' that
-- efficiently matches strings and moves on, but also
-- consumes at least some whitespace after each match.
switch_ws1 = switchWithPost (Just [|ws1|])

-- | Apply parser and then consume optional whitespace, comments,
-- or punctuation. Essentially, does not mark identifier boundary, but
-- will consume any space afterward. Useful for punctuation.
lx0 = (<* ws0)

-- | Apply parser and then consume some whitespace, comments,
-- or punctuation. Essentially, marks identifier boundary. Useful for
-- identifiers and keywords.
lx1 = (<* ws1)

-- | Parse @p@ but make sure @q@ cannot parse consuming the entirety
-- of the span of @p@.
butnot p q = withSpan p \x sp -> (inSpan sp (q >> eof) `fails`) $> x

-- | Parse @p@ but make sure @q@ is not a prefix of it.
butnotpfx p q = withSpan p \x sp -> (inSpan sp q `fails`) $> x
