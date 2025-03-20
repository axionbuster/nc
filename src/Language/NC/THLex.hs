-- due to a defect in FlatParse TH splice (generation of 'anykeyword')
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Language.NC.THLex where

import Language.NC.Prelude

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
           _ -> failed
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
           _ -> failed
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
           "//" -> consumeline
           "/*" -> findendcomment
           _ -> pure ()
         |]
   )

-- | Consume 0 or more horizontal spaces
hspace :: Parser ()
hspace = $(switch [|case _ of " " -> hspace; "\t" -> hspace; _ -> pure ()|])

consumeline, findendcomment :: Parser ()

-- | Consume line
consumeline = $(switch [|case _ of "\n" -> pure (); _ -> consumeline|])

-- | Find end of block comment (@*\/@)
findendcomment = $(switch [|case _ of "*/" -> pure (); _ -> findendcomment|])

doubleslash, startcomment, endcomment :: Parser ()
doubleslash = $(string "//")
startcomment = $(string "/*")
endcomment = $(string "*/")

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
           _ -> failed
         |]
   )

-- | Storage class specifier, one of
--   - @typedef@
--   - @extern@
--   - @static@
--   - @auto@
--   - @register@
storageclass :: Parser ByteString
storageclass = byteStringOf _storageclass

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
           "_Imaginary" -> pure ()
           _ -> failed
         |]
   )

-- | A single keyword that constitutes part of a primitive type specifier
primtypespec_word :: Parser ByteString
primtypespec_word = byteStringOf _primtypespec_word

struct, union, unionstruct, enum :: Parser ()
struct = $(string "struct")
union = $(string "union")
unionstruct = union >> ws >> struct
enum = $(string "enum")
