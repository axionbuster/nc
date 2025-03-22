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
unionstruct = union >> ws >> struct
enum = $(string "enum")

-- | Exported to support 'Language.NC.ParseDec.primtype'
_primtype :: Parser C.PrimType
_primtype =
  $( switch
       [|
         case _ of
           "void" -> pure C.Void
           "_Bool" -> pure C.Bool
           "signed" -> ws >> signed
           "unsigned" -> ws >> unsigned
           "float" -> ws >> float
           "double" -> ws >> double
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
                 ws
                   >> optional_ $(string "int")
                   $> C.Int C.Signed C.Short
               "long" -> ws >> long intonly
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
                 ws
                   >> optional_ $(string "int")
                   $> C.Int C.Signed C.LongLong
               "double" ->
                 if intonly
                   then err (PrimTypeBadError BecauseSignInLongDouble)
                   else ws >> double >>= mklongdouble
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
