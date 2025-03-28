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
--
-- Keywords are from C23 instead of C99, but the C99 keywords are still
-- included.
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
    lex,
    lex0,
    lx,
    lx0,

    -- * Data structures
    Constexpr99 (..),

    -- * Operators, keywords, and identifiers

    -- ** Brackets and parentheses
    ldbsqb,
    rdbsqb,
    lsqb0,
    rsqb0,
    lpar,
    rpar,
    lcur,
    rcur,
    insqb0,
    indbsqb,
    inpar,
    incur,

    -- ** Operator-like symbols

    -- NO handling of lexeme boundaries, so these will match the first
    -- part of a longer operator. Also, these don't generate parse nodes,
    -- as they simply match sequences of characters.
    comma,
    semicolon,
    period,
    rarrow,
    doubleplus,
    doubleminus,
    ampersand,
    caret,
    bar,
    tilde,
    plus,
    minus,
    star,
    slash,
    starslash,
    percent,
    less,
    greater,
    lessequal,
    greaterequal,
    equalequal,
    notequal,
    bang,
    doubleamp,
    doublebar,
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
    doublelessequal,
    doublegreaterequal,
    colon,
    doublecolon,
    questionmark,
    hash',
    doublehash',
    backslash,
    tripledot,

    -- ** Compensating for deficient encodings
    lesscolon,
    colongreater,
    lesspercent,
    percentgreater,
    percentcolon,
    doublepercentcolon,

    -- ** Individual keywords (roughly C23)

    -- (also includes keywords from C99)

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
    _Alignas',
    _Alignof',
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
    _Static_assert',
    _Thread_local',

    -- ** Reserved identifiers with special meanings
    __func__',
    main',

    -- ** Notable identifiers (expanding list)
    memset',
    printf',

    -- ** Helping find 'bad' syntax for identifiers
    doubleunderscore,
    underscorecap,
    sbs_doubleunderscore,
    sbs_hasdunder,
    bs_doubleunderscore,
    bs_hasdunder,

    -- * Comments and whitespace
    ws,
    ws1,

    -- * Utility
    switch_ws,
    switch1_ws,
    butnot,
  )
where

import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as SBS
import Language.Haskell.TH.Syntax
import Language.NC.Experiment.PrimTypes qualified as C
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
  -- We have C23 keywords.
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
           "typeof_unequal" -> pure ()
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
  | -- | Enumeration constant, having @int@ type.
    EnumConst
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

enumeration_constant = identifier $> EnumConst

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
               "\\x" -> upto 4 hexdigit
               "\\" -> upto 6 octdigit
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

-- NOTE from now on... this module defines a **lexer**, not a **parser**,
-- meaning it's not responsible for parsing expressions. Indeed, most lexers as
-- defined above merely accept or reject a sequence of characters, rather
-- than building up a tree of expressions.
--
-- But Template Haskell splices take up a lot of time to compile, so we
-- define TH-based parsers and utilities here.

-- Some operators. NOTE: if you parse say '=', it will match the first
-- half of '==', and the same for other operators. This is not a problem
-- for the lexer, but it could be for the parser.

-- Parentheses, brackets, and braces
-- At the lexing stage, we ignore digraphs.

(lsqb, rsqb) = ($(char '['), $(char ']')) -- don't export; export insqb0 instead

(ldbsqb, rdbsqb) = ($(string "[["), $(string "]]"))

(lsqb0, rsqb0) = (lsqb `notFollowedBy` lsqb, rsqb `notFollowedBy` rsqb)

(lpar, rpar) = ($(char '('), $(char ')'))

(lcur, rcur) = ($(char '{'), $(char '}'))

-- | Enclose a thing in square brackets not followed by another square bracket.
insqb0 = between (lsqb0 >> ws) rsqb0

-- | Enclose a thing in double square brackets.
indbsqb = between (ldbsqb >> ws) rdbsqb

-- | Enclose a thing in parentheses.
inpar = between (lpar >> ws) rpar

-- | Enclose a thing in braces.
incur = between (lcur >> ws) rcur

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

(hash', doublehash') = ($(char '#'), $(string "##"))

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

-- | This is OK, because there's only one place to check and it's also
-- an area where string search is not efficient.
underscorecap = $(char '_') >> skipSatisfyAscii \c -> 'A' <= c && c <= 'Z'

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
switch_ws :: Q Exp -> Q Exp
switch_ws = switchWithPost (Just [|ws|])

-- | like 'switch', but after each match, consume some whitespace
switch1_ws :: Q Exp -> Q Exp
switch1_ws = switchWithPost (Just [|ws1|])

-- | Parse a thing and then require whitespace or end of file, which is
-- also consumed. If there's an error, annotate it with a span, log it, and
-- then rethrow the error. Use 'try' to suppress the error.
lex :: Parser a -> Parser a
lex p = do
  st <- getPos
  let apologize e = do
        en <- getPos
        es <- pserrors <$> ask
        modifyIORef es (:|> aenew e (Span st en))
        err e
  withError p apologize <* ws1

-- | Like 'lex', but it does not require whitespace after parsing.
-- This is useful for lexing tokens where whitespace is not required after
-- the token (e.g., in the case of identifiers or certain punctuators).
lex0 :: Parser a -> Parser a
lex0 p = do
  st <- getPos
  let apologize e = do
        en <- getPos
        es <- pserrors <$> ask
        modifyIORef es (:|> aenew e (Span st en))
        err e
  withError p apologize <* ws

-- | Like 'lex', but no error handling is performed. It only requires
-- whitespace after. This is good for performance since it doesn't allocate
-- closures.
lx :: Parser a -> Parser a
lx = (<* ws1)

-- | Like 'lex0', but no error handling is performed to not allocate
-- closures.
lx0 :: Parser a -> Parser a
lx0 = (<* ws)

-- | Parse @a@ but also make sure @q@ cannot parse (must consume span).
butnot :: Parser a -> Parser b -> Parser a
butnot p q = withSpan p \x sp -> ((inSpan sp q >> eof) `fails`) $> x
