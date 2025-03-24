-- due to a defect in FlatParse TH splice (generation of 'anykeyword')
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | Export various lexing utilities that rely on Template Haskell.
--
-- NOTE: Even functions that begin with an underscore (\_) are exported.
module Language.NC.THLex where

import Language.Haskell.TH.Syntax
import Language.NC.CTypes qualified as C
import Language.NC.Internal.Prelude

-- | Match any keyword (reserved word)
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
         |]
   )

-- | Official C99 punctuator, except @<:@, @:>@, @<%@, @%>@, @%:@ and @%:%:@
punctuator :: Parser ()
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
         |]
   )

-- | Consume whitespace and comments
ws :: Parser ()
ws =
  $( switch
       [|
         case _ of
           " " -> ws
           "\t" -> ws
           "\n" -> ws
           "\r" -> ws
           "//" -> consumeline
           "/*" -> findendcomment
           _ -> pure ()
         |]
   )

-- | Consume some whitespace and comments.
ws1 :: Parser ()
ws1 =
  $( switch
       [|
         case _ of
           " " -> ws
           "\t" -> ws
           "\n" -> ws
           "\r" -> ws
           "//" -> consumeline
           "/*" -> findendcomment
           _ -> eof
         |]
   )

-- | Consume 0 or more horizontal spaces
hspace :: Parser ()
hspace = $(switch [|case _ of " " -> hspace; "\t" -> hspace; _ -> pure ()|])

consumeline, findendcomment :: Parser ()

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

doubleslash, startcomment, endcomment :: Parser ()

-- | Match a @\/\/@
doubleslash = $(string "//")

-- | Match a @\/*@
startcomment = $(string "/*")

-- | Match a @*\/@
endcomment = $(string "*/")

-- | Storage class specifier, one of
--   - @typedef@
--   - @extern@
--   - @static@
--   - @auto@
--   - @register@
_storageclass :: Parser ()
_storageclass =
  $( switch
       [|
         case _ of
           "typedef" -> pure ()
           "extern" -> pure ()
           "static" -> pure ()
           "auto" -> pure ()
           "register" -> pure ()
         |]
   )

-- | Storage class specifier, one of
--   - @typedef@
--   - @extern@
--   - @static@
--   - @auto@
--   - @register@
storageclass :: Parser Span
storageclass = spanOf _storageclass

-- | Match any of the keywords used to define a primitive nonderived type
_primtypespec_word :: Parser ()
_primtypespec_word =
  $( switch
       [|
         case _ of
           "int" -> pure ()
           "char" -> pure ()
           "void" -> pure ()
           "unsigned" -> pure ()
           "long" -> pure ()
           "double" -> pure ()
           "float" -> pure ()
           "signed" -> pure ()
           "short" -> pure ()
           "_Bool" -> pure ()
           "_Complex" -> pure ()
         |]
   )

-- | A single keyword that constitutes part of a primitive type specifier
primtypespec_word :: Parser Span
primtypespec_word = spanOf _primtypespec_word

struct, union, unionstruct, enum :: Parser ()
struct = $(string "struct")
union = $(string "union")
unionstruct = union >> ws1 >> struct
enum = $(string "enum")

-- | Exported to support 'Language.NC.ParseDec.primtype'
_primtype :: Parser C.PrimType
_primtype =
  $( switch
       [|
         case _ of
           "void" -> pure C.Void
           "_Bool" -> pure C.Bool
           "signed" -> ws1 >> signed
           "unsigned" -> ws1 >> unsigned
           "float" -> ws1 >> float
           "double" -> ws1 >> double
           _ -> zero False
         |]
   )
  where
    zero intonly =
      $( switch
           [|
             case _ of
               "char" -> pure C.Char_
               "int" -> pure C.Int_
               "short" ->
                 ws1
                   >> optional_ $(string "int")
                   $> C.Int C.Signed C.Short
               "long" -> ws1 >> long intonly
               "float" -> err (PrimTypeBadError BecauseSignNotMeaningful)
               "double" -> err (PrimTypeBadError BecauseSignNotMeaningful)
               "_Bool" -> err (PrimTypeBadError BecauseSignNotMeaningful)
               "void" -> err (PrimTypeBadError BecauseSignNotMeaningful)
               _ ->
                 if intonly
                   then pure C.Int_ -- called from signed/unsigned
                   else failed
             |]
       )
    long intonly =
      $( switch
           [|
             case _ of
               "long" ->
                 ws1
                   >> optional_ $(string "int")
                   $> C.Int C.Signed C.LongLong
               "double" ->
                 if intonly
                   then err (PrimTypeBadError BecauseSignInLongDouble)
                   else ws1 >> double >>= mklongdouble
               _ -> optional_ $(string "int") $> C.Int C.Signed C.Long
             |]
       )
    mklongdouble = \case
      C.Float (C.Real _) -> pure $ C.Float (C.Real C.RFLongDouble)
      C.Float (C.Complex _) -> pure $ C.Float (C.Complex C.RFLongDouble)
      _ -> err (InternalError "mklongdouble on neither float nor double")
    signed = zero True >>= sgn C.Signed
    unsigned = zero True >>= sgn C.Unsigned
    float = branch $(string "_Complex") (pure c) (pure r)
      where
        r = C.Float $ C.Real C.RFFloat; c = C.Float $ C.Complex C.RFFloat
    double = branch $(string "_Complex") (pure c) (pure r)
      where
        r = C.Float $ C.Real C.RFDouble; c = C.Float $ C.Complex C.RFDouble
    sgn s = \case
      C.Int _ l -> pure $ C.Int s l
      C.Char _ -> pure $ C.Char (Just s)
      _ -> err (PrimTypeBadError BecauseSignNotMeaningful)

-- | like 'switch', but after each match, consume any whitespace
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

-- * Various symbols

lbra, rbra, lpar, rpar, lsqbra, rsqbra :: Parser ()
lbra = $(char '{')
rbra = $(char '}')
lpar = $(char '(')
rpar = $(char ')')
lsqbra = $(char '[')
rsqbra = $(char ']')

inbra, inpar, insqbra :: Parser a -> Parser a
inbra = between lbra rbra
inpar = between lpar rpar
insqbra = between lsqbra rsqbra

comma, colon, semicolon :: Parser ()
comma = $(char ',')
colon = $(char ':')
semicolon = $(char ';')

star, amp, dot :: Parser ()
star = $(char '*')
amp = $(char '&')
dot = $(char '.')

rarr, equ_, equ0 :: Parser ()
rarr = $(string "->")
equ_ = $(char '=') <* ws1
equ0 = $(char '=')

void', sizeof, offsetof :: Parser ()
void' = $(string "void") <* ws1
sizeof = $(string "sizeof")
offsetof = $(string "offsetof")
