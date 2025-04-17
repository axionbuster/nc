{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- |
-- Module: NC.Parser.Decl
-- Description: Parse types and declarations
--
-- C type and declaration syntax are highly coupled by design, so it makes
-- sense to parse them both in the same module.
module NC.Parser.Decl (
  -- * Types
  typename,

  -- * Attributes
  attributes,

  -- * Declarations
  declspec,
  declaration,
  declarator,
  absdeclarator,
  anydeclarator,
  bracedinitializer,
  initializer,
) where

import NC.Internal.Prelude1 hiding (enum)
import {-# SOURCE #-} NC.Parser.Expr

-- * Internal

-- | Type specifiers and qualifiers and alignment specifiers.
newtype SQA = SQA Word64
  -- Num instance allows integer literals to be used, but it is
  -- otherwise nonsense.
  deriving (Eq, Show, Bits, Num)
  deriving (Monoid, Semigroup) via (Ior SQA)

-- | Extra data for 'T'
data Extra
  = -- | @_BitInt@
    ExBI !BitSize
  | -- | @struct@ or @union@
    ExRec !RecInfo
  | -- | @enum@
    ExEnum !EnumInfo
  | -- | @_Atomic(...)@ newtype
    ExAtomic !Type
  | -- | @typedef@ name. A @typedef@ definition is given as
    -- as a formal storage class specifier, instead. This is
    -- for using a @typedef@ name.
    ExTypedef !Type
  | -- | @typeof@ and @typeof_unqual@
    ExTypeof !Typeof !TypeofQual

-- | Token list for commutative tokens parsing.
data T = T
  { -- | Final specifier or qualifier.
    _t_sqa :: {-# UNPACK #-} !SQA,
    -- | This specifier or qualifier was repeated too often.
    _t_dups :: {-# UNPACK #-} !SQA,
    -- | Any explicit @alignof@ directive.
    _t_align :: !(Maybe Alignment),
    -- | Extra data.
    _t_extra :: !(Maybe Extra),
    -- | Optional attributes can go at the end of a @specifier-qualifier-list@.
    _t_attrs :: ![Attribute],
    -- | Other messages collected
    _t_msgs :: ![Message]
  }

-- | Initial value for 'T'
_t_0 :: T
_t_0 = T 0 0 Nothing Nothing [] []

-- | A transformation on 'T'.
newtype TT = TT {aptt :: T -> T}
  deriving (Monoid, Semigroup) via (Endo T)

makeLenses ''T

-- | Create a boolean mask lens.
__bool :: (Bits a) => a -> Lens' a Bool
__bool mask = lens g s
 where
  g = (/= (mask .^. mask)) . (.&. mask)
  s x = \case True -> x .|. mask; _ -> x .&. complement mask

sqa_const, sqa_restrict, sqa_volatile, sqa_atomic :: Lens' SQA Bool
sqa_const = __bool 0x01
sqa_restrict = __bool 0x02
sqa_volatile = __bool 0x04
sqa_atomic = __bool 0x08

sqa_extern, sqa_auto, sqa_static, sqa_typedef :: Lens' SQA Bool
sqa_extern = __bool 0x10
sqa_auto = __bool 0x20
sqa_static = __bool 0x40
sqa_typedef = __bool 0x80

sqa_thread_local, sqa_constexpr, sqa_register, sqa_int :: Lens' SQA Bool
sqa_thread_local = __bool 0x100
sqa_constexpr = __bool 0x200
sqa_register = __bool 0x400
sqa_int = __bool 0x800

sqa_bool, sqa_char, sqa_short, sqa_long :: Lens' SQA Bool
sqa_bool = __bool 0x1000
sqa_char = __bool 0x2000
sqa_short = __bool 0x4000
sqa_long = __bool 0x8000

sqa_longlong, sqa_bitint, sqa_float, sqa_double :: Lens' SQA Bool

-- | @long@ and @long long@ are treated specially. It's permissible
-- for @long@ to occur at most twice. So, if @long long@ is seen, I
-- want @long@ to test positive. Thus, I made this to set sqa_long as well.
sqa_longlong = __bool 0x1_8000

sqa_bitint = __bool 0x2_0000

sqa_float = __bool 0x4_0000

sqa_double = __bool 0x8_0000

sqa_complex, sqa_d32, sqa_d64, sqa_d128 :: Lens' SQA Bool
sqa_complex = __bool 0x10_0000
sqa_d32 = __bool 0x20_0000
sqa_d64 = __bool 0x40_0000
sqa_d128 = __bool 0x80_0000

sqa_void, sqa_nullptr, sqa_struct, sqa_union :: Lens' SQA Bool
sqa_void = __bool 0x100_0000
sqa_nullptr = __bool 0x200_0000
sqa_struct = __bool 0x400_0000
sqa_union = __bool 0x800_0000

sqa_enum, sqa_signed, sqa_unsigned, sqa_inline :: Lens' SQA Bool
sqa_enum = __bool 0x1000_0000
sqa_signed = __bool 0x2000_0000
sqa_unsigned = __bool 0x4000_0000
sqa_inline = __bool 0x8000_0000

sqa_noreturn, sqa_alignas, sqa_imaginary, sqa_typeof :: Lens' SQA Bool
sqa_noreturn = __bool 0x1_0000_0000
sqa_alignas = __bool 0x2_0000_0000
sqa_imaginary = __bool 0x4_0000_0000
sqa_typeof = __bool 0x8_0000_0000

sqa_typeof_unqual, sqa_atomicnewtype, sqa_typedefname :: Lens' SQA Bool
sqa_typeof_unqual = __bool 0x10_0000_0000
sqa_atomicnewtype = __bool 0x20_0000_0000
sqa_typedefname = __bool 0x40_0000_0000

-- | Collect \'specifier-qualifier-list\' into a 'Type'. This
-- involves applying 'specqualalign' many times.
specqualaligns :: P Type
specqualaligns = undefined

-- | Parse a single type-specifier, type-qualifier, or alignment-specifier.
-- (All these rules are merged to share bulk reading).
specqualalign :: P TT
specqualalign =
  $( switch_ws1
       [|
         case _ of
           "void" -> push sqa_void
           "char" -> push sqa_char
           "short" -> push sqa_short
           "int" -> push sqa_int
           "long" -> push sqa_long
           "float" -> push sqa_float
           "double" -> push sqa_double
           "signed" -> push sqa_signed
           "unsigned" -> push sqa_unsigned
           "_BitInt" -> bitint
           "_Bool" -> push sqa_bool
           "_Complex" -> push sqa_complex
           "_Decimal32" -> push sqa_d32
           "_Decimal64" -> push sqa_d64
           "_Decimal128" -> push sqa_d128
           "_Atomic" -> atomic
           "struct" -> struct
           "union" -> union
           "enum" -> enum
           "typeof" -> typeof
           "typeof_unqual" -> typeof_unqual
           "alignas" -> alignas
           "_Alignas" -> alignas
           "const" -> push sqa_const
           "restrict" -> push sqa_restrict
           "volatile" -> push sqa_volatile
           _ -> typedefname
         |]
   )
 where
  push (tok :: Lens' SQA Bool) = pure $ TT \tokens ->
    -- test: been set already? generally duplicate tokens are bad.
    if tokens ^. t_sqa . tok
      then -- duplicate, check for one exception (long long).
        if ((mempty & tok .~ True) == (mempty & sqa_long .~ True))
          && not (tokens ^. t_sqa . sqa_longlong)
          then -- oh, this is an exception.
            tokens & t_sqa . sqa_longlong .~ True
          else -- most likely case. log error.
            tokens & t_dups . tok .~ True
      else -- no problem, not been set before.
        tokens & t_sqa . tok .~ True
  bitint = do
    -- FIXME: we need to admit a constant-expression, but we require
    -- a _BitInt type to only use an actual 14-bit integer. I think
    -- we'll need to redesign the primitive type system. For now,
    -- I'll use an integer literal, instead.
    (lpar `pcut_expect` "(")
      >> pcut' (MsgExpect "integer literal") do
        IntegerLiteral litnum _ <- integer_constant_val
        let ln2 = fromIntegral litnum
        let badbitsize = MsgAdHoc "Bitsize invalid or exceeds compiler limit"
        pure
          if (litnum <= 0)
            || (litnum > fromIntegral (maxBound :: Word16))
            || isNothing (bitintwidth_ok ln2)
            then TT \tokens -> tokens & t_msgs %~ cons badbitsize
            else TT \tokens -> tokens & t_extra .~ Just (ExBI ln2)
      <* (rpar `pcut_expect` ")")
  atomic = do
    -- it's either a "newtype" specifier if followed by an opening paren
    -- but otherwise it's just a qualifier.
    branch_inpar atonew atoqua
   where
    atonew = do
      tn <- typename `pcut_expect` "type name"
      pure
        $ TT (t_extra .~ Just (ExAtomic tn))
        <> TT (t_sqa . sqa_atomicnewtype .~ True)
    atoqua = push sqa_atomic
  struct = (<>) <$> push sqa_struct <*> parserecord RecStruct
  union = (<>) <$> push sqa_union <*> parserecord RecUnion
  enum = (<>) <$> push sqa_enum <*> parseenum
  typeof = typeof2 TQQual
  typeof_unqual = typeof2 TQUnqual
  alignas =
    (<>) <$> push sqa_alignas <*> do
      lpar `pcut_expect` "(" >> do
        t <-
          ((AlignAsType <$> typename) <|> (AlignAs <$> constexpr))
            `pcut_expect` "a type name or a constant expression"
        pure (TT (t_align .~ Just t)) <* rpar `pcut_expect` ")"
  typedefname = (<>) <$> push sqa_typedefname <*> undefined identifier
  typeof2 qualification =
    let lens2use :: Lens' SQA Bool
        lens2use
          | TQQual <- qualification = sqa_typeof
          | otherwise = sqa_typeof_unqual
     in (<>)
          <$> push lens2use
          <*> do
            t <- (TypeofType <$> typename) <|> (TypeofExpr <$> expr)
            pure $ TT (t_extra .~ Just (ExTypeof t qualification))

-- | Parse a record type and then return a type transformer (TT).
parserecord :: RecType -> P TT
parserecord = doparse >=> change
 where
  change :: RecInfo -> P TT
  change ty = pure $ TT (t_extra .~ Just (ExRec ty))
  doparse :: RecType -> P RecInfo
  doparse rt = do
    sym <- symnew
    withOption identifier_def (`symgivetypetag` sym) (pure ())
    RecInfo rt sym <$> attrspecs0 <*> do
      optional do
        concat
          <$> flip endBy semicolon do
            -- we allow an empty struct/union declaration, which is a deviation
            -- from the standard C23 grammar.
            branch static_assert' (pure . RMStaticAssertion <$> parsesabody) do
              sym <- symnew
              conrec <- RMField <$> attrspecs0 <*> pure sym
              basetype <- specqualaligns
              flip sepBy comma do
                dclr <- optional $ declarator sym
                bitwidth <- optional $ colon >> constexpr
                case bitwidth of
                  Nothing -> case dclr of
                    Just de -> pure $ conrec (apdecl de basetype) Nothing
                    Nothing -> pthrow (MsgExpect "expected member type") ""
                  Just bw -> do
                    -- a field that specifies a bit width does not need
                    -- to specify a declarator.
                    let de = fromMaybe mempty dclr
                    let type2 = apdecl de basetype
                    pure $ conrec type2 (Just bw)

-- | Parse the body that follows a @static_assert@ token. This includes
-- the semicolon at the end.
parsesabody :: P StaticAssertion
parsesabody = inpar body <* semicolon
 where
  body = StaticAssertion <$> constexpr <*> optional msg
  msg = comma >> string_literal_val

parseenum :: P TT
parseenum = undefined

-- | Parse a single attribute specifier (@[[ ... ]]@). Here, a single specifier
-- can actually introduce many attributes.
attrspec :: P [Attribute]
attrspec = ldbsqb >> attr `sepBy1` comma <* rdbsqb
 where
  attr = do
    i <- identifier
    withOption
      (dbcolon >> identifier `pcut_expect` "identifier")
      (pure . Attribute (Just i))
      (pure $ Attribute Nothing i)
      <*> do
        fmap (view (from span64)) <$> optional do
          spanOf do
            inpar do
              let stuff =
                    $( switch
                         [|
                           case _ of
                             "(" -> pure ()
                             "[" -> pure ()
                             "<:" -> pure ()
                             "{" -> pure ()
                             ")" -> pure ()
                             "]" -> pure ()
                             ":>" -> pure ()
                             "}" -> pure ()
                           |]
                     )
              $( switch_ws0
                   [|
                     case _ of
                       "(" -> skipMany stuff <* rpar `pcut_expect` ")"
                       "[" -> skipMany stuff <* rsqb `pcut_expect` "]"
                       "<:" -> skipMany stuff <* rsqb `pcut_expect` "]"
                       "{" -> skipMany stuff <* rpar `pcut_expect` "}"
                       _ -> skipMany stuff
                     |]
               )

attrspecs0, attrspecs1 :: P [Attribute]

-- | Parse 0 or more attribute specifiers.
attrspecs0 = chainr (<>) attrspec (pure mempty)

-- | Parse 1 or more attribute specifiers.
attrspecs1 = do
  l <- attrspecs0
  guard (not . null $ l) $> l

-- * Exported

-- ** Types

typename = undefined

-- ** Attributes

attributes = undefined

-- ** Declarations

declspec = undefined

declaration = undefined

-- | Create a declarator, which introduces an identifier. Associate the
-- identifier with the symbol.
declarator :: Symbol -> P Declarator
declarator = undefined

absdeclarator = undefined

anydeclarator = undefined

bracedinitializer = undefined

initializer = undefined
