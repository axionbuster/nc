{-# OPTIONS_GHC -Wno-name-shadowing -Wno-missing-signatures #-}

-- | Lexers.
--
-- Lexing convention:
--  - Each parser\/lexer assumes the respective token begins directly rather
--    than potentially starting with whitespace. Consequently, these primitives
--    do not begin with whitespace parsing.
--  - It also does not consume whitespace after itself.
module Language.NC.Internal.Lex where

import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as SBS
import Data.Text.ICU.Char qualified as C
import GHC.Int
import GHC.Integer.Logarithms (integerLog2#)
import Language.NC.Internal.Prelude
import Language.NC.Internal.PrimTypes qualified as PT

-- * Whitespace and comments

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
           _ -> nomat
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

-- | Some or no whitespace or comments
ws0 = ws_base (pure ())

-- | At least some whitespace or comments
ws1 = ws_base eof

-- * Operators, keywords, and identifiers

-- | @[@
_lsq = $(char '[') <|> $(string "<:")

-- | @]@
_rsq = $(char ']') <|> $(string ":>")

-- | @[[@ and @]]@, respectively.
(ldbsqb, rdbsqb) =
  -- The C standard allows whitespace to go between them.
  ( _lsq >> ws0 >> _lsq,
    _rsq >> ws0 >> _rsq
  )

-- | These check that they're not followed by another copy of the respective
-- bracket (e.g., @[[@, @]]@).
(lsqb, rsqb) =
  ( (_lsq >> ws0) `notFollowedBy` _lsq,
    (_rsq >> ws0) `notFollowedBy` _rsq
  )

(lpar, rpar) = ($(char '('), $(char ')'))

(lcur, rcur) = ($(char '{') <|> $(string "<%"), $(char '}') <|> $(string "%>"))

-- | Enclose a thing in square brackets not followed by another square bracket.
insqb0 = between (lsqb >> ws0) (ws0 >> rsqb)

-- | Enclose a thing in double square brackets.
indbsqb = between (ldbsqb >> ws0) (ws0 >> rdbsqb)

-- | Enclose a thing in parentheses.
inpar = between (lpar >> ws0) (ws0 >> rpar)

-- | Enclose a thing in braces.
incur = between (lcur >> ws0) (ws0 >> rcur)

-- Important punctuations

(comma, semicolon) = ($(char ','), $(char ';'))

-- Member access operators
(period, rarrow) = ($(char '.'), $(string "->"))

-- Increment/decrement operators
(doubleplus, doubleminus) = ($(string "++"), $(string "--"))

-- Bitwise operators
(ampersand, caret, bar, tilde) =
  ($(char '&'), $(char '^'), $(char '|'), $(char '~'))

-- Basic arithmetic operators
(plus, minus, star, slash, percent) =
  ($(char '+'), $(char '-'), $(char '*'), s, $(char '%'))
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

starslash = $(string "*/")

-- Relational operators
(less, greater) = ($(char '<'), $(char '>'))

(lessequal, greaterequal) = ($(string "<="), $(string ">="))

(equalequal, notequal) = ($(string "=="), $(string "!="))

-- Logical operators
(bang, doubleamp, doublebar) = ($(char '!'), $(string "&&"), $(string "||"))

-- Assignment operator
equal = $(char '=')

-- Compound assignments
(starequal, slashequal, percentequal, plusequal, minusequal) =
  ( $(string "*="),
    $(string "/="),
    $(string "%="),
    $(string "+="),
    $(string "-=")
  )

(ampersandequal, caretequal, barequal, tildeequal) =
  ( $(string "&="),
    $(string "^="),
    $(string "|="),
    $(string "~=")
  )

(doublelessequal, doublegreaterequal) =
  ( $(string "<<="),
    $(string ">>=")
  )

-- ** Misc operators (and parts of operators)

(colon, doublecolon) = ($(char ':'), $(string "::"))

questionmark = $(char '?')

-- ** Preprocessor 'operators'

(hash', doublehash') =
  ( $(char '#') <|> $(string "%:"),
    hash' >> hash'
  )

backslash = $(char '\\')

-- ** Ellipsis

tripledot = $(string "...")

-- ** Compensation for deficient encodings (digraphs)

-- square brackets
(lesscolon, colongreater) = ($(string "<:"), $(string ":>"))

-- braces
(lesspercent, percentgreater) = ($(string "<%"), $(string "%>"))

-- single and double hashes
(percentcolon, doublepercentcolon) = ($(string "%:"), $(string "%:%:"))

-- ** Keywords

-- Same warning applies as above; they aren't defined as lexemes (yet)
-- so it will be insensitive to whitespace, lookahead is not handled, etc.
-- Basically no semantic grouping is done here.

alignas' = $(string "alignas")

alignof' = $(string "alignof")

auto' = $(string "auto")

break' = $(string "break")

case' = $(string "case")

char' = $(string "char")

const' = $(string "const")

constexpr' = $(string "constexpr")

continue' = $(string "continue")

default' = $(string "default")

do' = $(string "do")

double' = $(string "double")

else' = $(string "else")

enum' = $(string "enum")

extern' = $(string "extern")

false' = $(string "false")

float' = $(string "float")

for' = $(string "for")

goto' = $(string "goto")

if' = $(string "if")

inline' = $(string "inline")

int' = $(string "int")

long' = $(string "long")

nullptr' = $(string "nullptr")

nullptr_t' = $(string "nullptr_t")

register' = $(string "register")

restrict' = $(string "restrict")

return' = $(string "return")

short' = $(string "short")

signed' = $(string "signed")

sizeof' = $(string "sizeof")

static' = $(string "static")

static_assert' = $(string "static_assert")

struct' = $(string "struct")

switch' = $(string "switch")

thread_local' = $(string "thread_local")

true' = $(string "true")

typedef' = $(string "typedef")

typeof' = $(string "typeof")

typeof_unequal' = $(string "typeof_unequal")

union' = $(string "union")

unsigned' = $(string "unsigned")

void' = $(string "void")

volatile' = $(string "volatile")

while' = $(string "while")

_Alignas' = $(string "_Alignas")

_Alignof' = $(string "_Alignof")

_Atomic' = $(string "_Atomic")

_BitInt' = $(string "_BitInt")

_Bool' = $(string "_Bool")

_Complex' = $(string "_Complex")

_Decimal128' = $(string "_Decimal128")

_Decimal32' = $(string "_Decimal32")

_Decimal64' = $(string "_Decimal64")

_Generic' = $(string "_Generic")

_Imaginary' = $(string "_Imaginary")

_Noreturn' = $(string "_Noreturn")

_Static_assert' = $(string "_Static_assert")

_Thread_local' = $(string "_Thread_local")

-- ** some reserved identifiers.

-- user code may not declare these identifiers. they have special
-- meaning in the C standard.

__func__' = $(string "__func__")

main' = $(string "main")

-- ** Any notable identifiers.

-- this is an ever expanding list of identifiers that are not keywords
-- or reserved identifiers, but are notable in some way.

memset' = $(string "memset")

printf' = $(string "printf")

-- ** Helping find 'bad' syntax for identifiers.

-- They are allowed by the standard, but are not recommended except
-- in special circumstances (e.g., standard library implementations):
--
--  - Identifier begins with an underscore followed
--    by an uppercase Latin letter.
--  - Identifier contains two consecutive underscores anywhere.

-- | AVOID, use efficient string search instead when possible.
doubleunderscore = $(string "__")

sbs_doubleunderscore = SBS.pack [95, 95]

sbs_hasdunder = (sbs_doubleunderscore `SBS.isInfixOf`)

bs_doubleunderscore = BS.pack [95, 95]

bs_hasdunder = (bs_doubleunderscore `BS.isInfixOf`)

-- * Lexing (6.4 Lexical elements)

-- ** Keywords

-- | C23 keywords
keyword =
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
identifier = id_head >> skipMany id_tail
  where
    id_head = nondigit <|> universal_character_name
    id_tail = fullset <|> universal_character_name
    nondigit = skipFusedSatisfy asciind xids xids xids
    fullset = skipFusedSatisfy asciifull xidc xidc xidc
    asciind = \c -> isLatinLetter c || c == '_'
    asciifull = \c -> isDigit c || isLatinLetter c || c == '_'
    xids = C.property C.XidStart
    xidc = C.property C.XidContinue

-- | A user-definable identifier. Currently, only @__func__@ is banned.
identifier_def =
  $( switch
       [|
         case _ of
           "__func__" -> failed
           _ -> identifier
         |]
   )

-- *** 6.4.3 Universal character names

universal_character_name = () <$ ucnam_val_word

-- | A Unicode character (in a 'Word')
ucnam_val_word =
  $( switch
       [|
         case _ of
           "\\u" -> isolate 4 anyAsciiHexWord
           "\\U" -> isolate 8 anyAsciiHexWord
         |]
   )
    >>= \c ->
      c <$ guard do
        (c < 0x00A0 && c `notElem` map (fromIntegral . ord) ['$', '@', '`'])
          || (0xD800 <= c && c <= 0xDFFF)
          || (c > 0x10FFFF)

-- *** 6.4.4 Constants

-- | Assemble a number from a base, parser, and max number of digits.
--
-- Set max number to a negative number to consume all digits.
--
-- Require at least one digit be consumed, or else fail.
asm b p = go 0 True
  where
    -- i make sure two separators (') can't follow each other.
    go n sepfresh = \case
      (0 :: Int) -> pure n
      i ->
        -- for simplicity i hard-code the digit separator.
        branch
          ($(char '\'') >> guard sepfresh)
          (go n False i)
          ( do
              d <- p
              let m = n * b + d
              go m True (i - 1) <|> pure m
          )

_hexdigit =
  satisfyAscii isHexDigit <&> fromIntegral . \c ->
    if
      | 'A' <= c && c <= 'F' -> ord c - ord 'A' + 10
      | 'a' <= c && c <= 'f' -> ord c - ord 'a' + 10
      | '0' <= c && c <= '9' -> ord c - ord '0'
      | otherwise -> error "_hexdigit: impossible"

_octdigit =
  satisfyAscii isOctDigit <&> fromIntegral . \c -> ord c - ord '0'

_decdigit =
  satisfyAscii isDigit <&> fromIntegral . \c -> ord c - ord '0'

_bindigit =
  $( switch
       [|
         case _ of
           "0" -> pure 0
           "1" -> pure 1
         |]
   )

data IntegerLiteral
  = IntegerLiteral Integer PT.PrimType
  deriving (Eq, Show)

data ISFX_
  = -- occurrence counts. we do a very basic check.
  ISFX_ {isfxu :: !Word8, isfxl :: !Word8, isfxbw :: !Word8}

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

-- find integral type that fits literal or else return 'Nothing'
isfx_2inttyp wasdecimal = gated_go . integerneededbits
  where
    i_ = PT.Int_
    l_ = PT.Long_
    ll_ = PT.LongLong_
    u_ = PT.UInt_
    ul_ = PT.ULong_
    ull_ = PT.ULongLong_
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
    go bw (ISFX_ 0 0 1) = pure $ PT.Int PT.Signed $ PT.BitInt $ min 2 bw
    go bw (ISFX_ 1 0 1) = pure $ PT.Int PT.Unsigned $ PT.BitInt bw
    go _ _ = err $ LiteralBadError IncorrectIntSuffix

-- used to compute bitwidth for _BitInt(N) literals.
-- as an invariant, this is > 1.
integerneededbits 0 = 1
integerneededbits x
  | x < 0 = I# (integerLog2# (negate x)) + 1 -- sign bit
  | otherwise = I# (integerLog2# x)

-- | Parse an integer constant and the value.
--
-- Two things to note:
--  - No sign parsing: it's done by the negation (-) operator.
--  - Automatic widening as per C23: the C23 standard prescribes
--    certain automatic widenings that must occur if the literal
--    is too big to fit in the specified (if any) suffix.
--  - No guard for trailing digits: so things like \"01138\"
--    will parse just fine and return the octal number 0113 (dec 75)
--    and input 8 that's left over. that's OK; you should use it with
--    a word boundary parser like 'lx1' to reject such cases.
integer_constant_val =
  $( switch
       [|
         case _ of
           "0x" -> hex
           "0X" -> hex
           "0b" -> bin
           "0B" -> bin
           "0" -> oct <|> pure (False, 0)
           _ -> dec
         |]
   )
    >>= \(wasdecimal, n) -> IntegerLiteral n <$> sfx wasdecimal n
  where
    -- the (-1) indicates max number of digits (unrestricted)
    hex = (False,) <$> asm 16 _hexdigit (-1)
    oct = (False,) <$> asm 8 _octdigit (-1)
    dec = (True,) <$> asm 10 _decdigit (-1)
    bin = (False,) <$> asm 2 _bindigit (-1)
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

-- | Create a Template Haskell splice like 'switch' that
-- efficiently matches strings and moves on, but also
-- optionally consumes whitespace after each match.
switch_ws0 = switchWithPost (Just [|ws0|])

-- | Create a Template Haskell splice like 'switch' that
-- efficiently matches strings and moves on, but also
-- consumes at least some whitespace after each match.
switch_ws1 = switchWithPost (Just [|ws1|])

-- | Lexeme ... apply parser and then consume optional whitespace
lx0 = (<* ws0)

-- | Lexeme ... apply parser and then consume whitespace
lx1 = (<* ws1)

-- | Parse with @p@; on error, record with span and then handle
-- it with @q@. Usage: @pcatch p q@.
pcatch p q = do
  st <- getPos
  let h e = do
        en <- getPos
        es <- pserrors <$> ask
        modifyIORef es (:|> aenew e (Span st en))
        q e
  withError p h

-- | Parse; on error, record error and then fail.
pfinally p = pcatch p (const failed)

-- | Parse; on error, record error and then rethrow.
p_onexception p = pcatch p err

-- | Parse @p@ but make sure @q@ cannot parse consuming the entirety
-- of the span of @p@.
butnot p q = withSpan p \x sp -> ((inSpan sp q >> eof) `fails`) $> x

-- | Parse @p@ but make sure @q@ is not a prefix of it.
butnotpfx p q = withSpan p \x sp -> (inSpan sp q `fails`) $> x
