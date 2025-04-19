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
  attrspecs0,
  attrspecs1,
  attrspec,

  -- * Declarations
  declaration,
  declarator,
  absdeclarator,
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
  g = (== mask) . (.&. mask)
  s x = \case True -> x .|. mask; _ -> x .&. complement mask

-- Pattern synonyms for SQA flags
{-# COMPLETE
  SqaConst,
  SqaRestrict,
  SqaVolatile,
  SqaAtomic,
  SqaExtern,
  SqaAuto,
  SqaStatic,
  SqaTypedef,
  SqaThreadLocal,
  SqaConstexpr,
  SqaRegister,
  SqaInt,
  SqaBool,
  SqaChar,
  SqaShort,
  SqaLong,
  SqaLongLong,
  SqaBitInt,
  SqaFloat,
  SqaDouble,
  SqaComplex,
  SqaD32,
  SqaD64,
  SqaD128,
  SqaVoid,
  SqaNullptr,
  SqaStruct,
  SqaUnion,
  SqaEnum,
  SqaSigned,
  SqaUnsigned,
  SqaInline,
  SqaNoreturn,
  SqaAlignas,
  SqaImaginary,
  SqaTypeof,
  SqaTypeofUnqual,
  SqaAtomicnewtype,
  SqaTypedefname
  #-}

pattern SqaConst, SqaRestrict, SqaVolatile, SqaAtomic :: SQA
pattern SqaConst = 1
pattern SqaRestrict = 2
pattern SqaVolatile = 4
pattern SqaAtomic = 8

pattern SqaExtern, SqaAuto, SqaStatic, SqaTypedef :: SQA
pattern SqaExtern = 0x10
pattern SqaAuto = 0x20
pattern SqaStatic = 0x40
pattern SqaTypedef = 0x80

pattern SqaThreadLocal, SqaConstexpr, SqaRegister, SqaInt :: SQA
pattern SqaThreadLocal = 0x100
pattern SqaConstexpr = 0x200
pattern SqaRegister = 0x400
pattern SqaInt = 0x800

pattern SqaBool, SqaChar, SqaShort, SqaLong :: SQA
pattern SqaBool = 0x1000
pattern SqaChar = 0x2000
pattern SqaShort = 0x4000
pattern SqaLong = 0x8000

pattern SqaLongLong, SqaBitInt, SqaFloat, SqaDouble :: SQA
-- @long@ and @long long@ are treated specially. It's permissible
-- for @long@ to occur at most twice. So, if @long long@ is seen, I
-- want @long@ to test positive. Thus, I made this to set SqaLong as well.
pattern SqaLongLong = 0x1_8000
pattern SqaBitInt = 0x2_0000
pattern SqaFloat = 0x4_0000
pattern SqaDouble = 0x8_0000

pattern SqaComplex, SqaD32, SqaD64, SqaD128 :: SQA
pattern SqaComplex = 0x10_0000
pattern SqaD32 = 0x20_0000
pattern SqaD64 = 0x40_0000
pattern SqaD128 = 0x80_0000

pattern SqaVoid, SqaNullptr, SqaStruct, SqaUnion :: SQA
pattern SqaVoid = 0x100_0000
pattern SqaNullptr = 0x200_0000
pattern SqaStruct = 0x400_0000
pattern SqaUnion = 0x800_0000

pattern SqaEnum, SqaSigned, SqaUnsigned, SqaInline :: SQA
pattern SqaEnum = 0x1000_0000
pattern SqaSigned = 0x2000_0000
pattern SqaUnsigned = 0x4000_0000
pattern SqaInline = 0x8000_0000

pattern SqaNoreturn, SqaAlignas, SqaImaginary, SqaTypeof :: SQA
pattern SqaNoreturn = 0x1_0000_0000
pattern SqaAlignas = 0x2_0000_0000
pattern SqaImaginary = 0x4_0000_0000
pattern SqaTypeof = 0x8_0000_0000

pattern SqaTypeofUnqual, SqaAtomicnewtype, SqaTypedefname :: SQA
pattern SqaTypeofUnqual = 0x10_0000_0000
pattern SqaAtomicnewtype = 0x20_0000_0000
pattern SqaTypedefname = 0x40_0000_0000

-- | This will never match.
pattern SqaOther :: SQA
pattern SqaOther = 0xffff_ffff_ffff_ffff

-- | Collect \'specifier-qualifier-list\' into a 'Type'. This
-- involves applying 'specqualalign' many times.
specqualaligns :: P Type
specqualaligns = undefined

-- | We use a case-of match instead of a direct string match because
-- this portion is shared between different parsers. We want an efficient
-- trie search which favors the case where every possibility is combined
-- and tested at once.
specqualalign_real :: SQA -> P TT
specqualalign_real = \case
  SqaVoid -> push SqaVoid
  SqaChar -> push SqaChar
  SqaShort -> push SqaShort
  SqaInt -> push SqaInt
  SqaLong -> push SqaLong
  SqaFloat -> push SqaFloat
  SqaDouble -> push SqaDouble
  SqaSigned -> push SqaSigned
  SqaUnsigned -> push SqaUnsigned
  SqaBitInt -> bitint
  SqaBool -> push SqaBool
  SqaComplex -> push SqaComplex
  SqaD32 -> push SqaD32
  SqaD64 -> push SqaD64
  SqaD128 -> push SqaD128
  SqaAtomic -> atomic
  SqaStruct -> struct
  SqaUnion -> union
  SqaEnum -> enum
  SqaTypeof -> typeof
  SqaTypeofUnqual -> typeof_unqual
  SqaAlignas -> alignas
  SqaConst -> push SqaConst
  SqaRestrict -> push SqaRestrict
  SqaVolatile -> push SqaVolatile
  _nomatch -> do
    traceIO "handling typedefname"
    typedefname
 where
  s %&~ b = s %~ (.|. b)
  push sqa = pure $ coerce \rt -> do
    let rts = rt ^. t_sqa
    if sqa .&. rts == sqa
      then -- duplicate, unless long and no long long
        if sqa == SqaLong && sqa .&. SqaLongLong == SqaLongLong
          then -- oh, it's fine
            rt & t_sqa %&~ sqa
          else -- oops, bad
            rt & t_dups %&~ sqa
      else
        -- admit token
        rt & t_sqa %&~ sqa
  bitint = do
    -- FIXME: we need to admit a constant-expression, but we require
    -- a _BitInt type to only use an actual 16-bit integer. I think
    -- we'll need to redesign the primitive type system. For now,
    -- I'll use an integer literal, instead. UPDATE: There's no need to
    -- update the primitive type itself. converting it to an expression
    -- that's potentially unevaluated is more trouble than its worth.
    -- rather, it's probably a better idea to enable evaluation here.
    -- i'll touch on this issue later.
    cutlpar
      >> pcut' (MsgExpect "integer literal") do
        IntegerLiteral litnum _ <- integer_constant_val
        let ln2 = fromIntegral litnum
        let badbitsize = MsgAdHoc "Bitsize invalid or exceeds compiler limit"
        pure
          if (litnum <= 0)
            || (litnum > fromIntegral (maxBound :: Word16))
            then TT (t_msgs %~ cons badbitsize)
            else TT (t_extra .~ Just (ExBI ln2))
      <* cutrpar
  atomic = do
    -- it's either a "newtype" specifier if followed by an opening paren
    -- but otherwise it's just a qualifier.
    branch_inpar atonew atoqua
   where
    atonew = do
      tn <- typename `pcut_expect` "type name"
      pure
        $ TT (t_extra .~ Just (ExAtomic tn))
        <> TT (t_sqa %&~ SqaAtomicnewtype)
    atoqua = push SqaAtomic
  struct = (<>) <$> push SqaStruct <*> parserecord RecStruct
  union = (<>) <$> push SqaUnion <*> parserecord RecUnion
  enum = (<>) <$> push SqaEnum <*> parseenum
  typeof = typeof2 TQQual
  typeof_unqual = typeof2 TQUnqual
  alignas =
    (<>) <$> push SqaAlignas <*> do
      cutlpar >> do
        t <-
          ((AlignAsType <$> typename) <|> (AlignAs <$> constexpr))
            `pcut_expect` "a type name or a constant expression"
        pure (TT (t_align .~ Just t)) <* cutrpar
  typedefname = (<>) <$> push SqaTypedefname <*> undefined identifier
  typeof2 qualification =
    let field2use
          | TQQual <- qualification = SqaTypeof
          | otherwise = SqaTypeofUnqual
     in (<>)
          <$> push field2use
          <*> do
            t <- (TypeofType <$> typename) <|> (TypeofExpr <$> expr)
            pure $ TT (t_extra .~ Just (ExTypeof t qualification))

-- | Parse a single type-specifier, type-qualifier, or alignment-specifier.
-- (All these rules are merged to share bulk reading).
specqualalign :: P TT
specqualalign =
  $( switch_ws1
       [|
         case _ of
           "void" -> r SqaVoid
           "char" -> r SqaChar
           "short" -> r SqaShort
           "int" -> r SqaInt
           "long" -> r SqaLong
           "float" -> r SqaFloat
           "double" -> r SqaDouble
           "signed" -> r SqaSigned
           "unsigned" -> r SqaUnsigned
           "_BitInt" -> r SqaBitInt
           "_Bool" -> r SqaBool
           "_Complex" -> r SqaComplex
           "_Decimal32" -> r SqaD32
           "_Decimal64" -> r SqaD64
           "_Decimal128" -> r SqaD128
           "_Atomic" -> r SqaAtomic
           "struct" -> r SqaStruct
           "union" -> r SqaUnion
           "enum" -> r SqaEnum
           "typeof" -> r SqaTypeof
           "typeof_unqual" -> r SqaTypeofUnqual
           "_Alignas" -> r SqaAlignas
           "const" -> r SqaConst
           "restrict" -> r SqaRestrict
           "volatile" -> r SqaVolatile
           _ -> r SqaOther
         |]
   )
 where
  r = specqualalign_real

-- | Parse a record type and then return a type transformer (TT).
parserecord :: RecType -> P TT
parserecord = doparse >=> change
 where
  change :: RecInfo -> P TT
  change ty = pure $ TT (t_extra .~ Just (ExRec ty))
  doparse :: RecType -> P RecInfo
  doparse rt = do
    sym <- symnew
    withOption identifier_def (symgivetypetag sym) (pure ())
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
                bitwidth <-
                  optional
                    $ colon
                    >> (constexpr `pcut_expect` "a constant expression")
                case bitwidth of
                  Nothing -> case dclr of
                    Just de -> pure $ conrec (apdecl de basetype) Nothing
                    Nothing -> pexpect "expected member type"
                  Just bw -> do
                    -- a field that specifies a bit width does not need
                    -- to specify a declarator.
                    let de = fromMaybe mempty dclr
                    let type2 = apdecl de basetype
                    pure $ conrec type2 (Just bw)

-- | Parse the body that follows a @static_assert@ token. This includes
-- the semicolon at the end.
parsesabody :: P StaticAssertion
parsesabody = inpar body <* cutsemicolon
 where
  body = StaticAssertion <$> constexpr <*> optional msg
  msg = comma >> string_literal_val

-- | Parse an @enum@ body.
parseenum :: P TT
parseenum = doparse >>= change
 where
  change ty = pure $ TT (t_extra .~ Just (ExEnum ty))
  doparse = do
    sym <- symnew
    attrs <- attrspecs0
    name <- optional identifier
    membertype <-
      option
        (pr2type pr_int)
        ( colon
            >> specqualaligns
            `pcut_expect` "type specifiers and qualifiers \
                          \and alignment specifiers"
        )
    let members =
          EnumInfo sym attrs membertype
            . Just
            <$> flip sepEndBy comma do
              sym <- symnew
              identifier_def >>= (symgivegeneralname sym)
              EnumConst sym <$> attrspecs0 <*> optional do
                cutequal >> constexpr
        incomplete = do
          -- an incomplete enum declaration has some restrictions.
          -- TODO: throw multiple errors at once.
          if
            | not (null attrs) -> pexpect "no attributes"
            | isNothing name -> pexpect "a tag"
            | otherwise ->
                pure
                  $ EnumInfo
                    sym
                    attrs
                    membertype
                    Nothing
    branch_incur members incomplete

-- | Parse the pointer past the star (@*@).
pointerbody :: P Declarator
pointerbody = undefined

-- * Exported

-- ** Types

-- | Parse the @type-name@ rule, producing a 'Type'.
typename :: P Type
typename = specqualaligns <**> (apdecl <$> absdeclarator)

-- ** Attributes

attrspecs0, attrspecs1 :: P [Attribute]

-- | Parse 0 or more attribute specifiers.
attrspecs0 = chainr (<>) attrspec (pure mempty)

-- | Parse 1 or more attribute specifiers.
attrspecs1 = attrspecs0 >>= \l -> guard (not . null $ l) $> l

-- | Parse a single attribute specifier (@[[ ... ]]@). Here, a single specifier
-- can actually introduce many attributes.
attrspec :: P [Attribute]
attrspec = ldbsqb >> attr `sepBy1` comma <* cutrdbsqb
 where
  attr = do
    i <- identifier
    branch
      dbcolon
      (Attribute (Just i) <$> identifier `pcut_expect` "identifier")
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
              $( switch
                   [|
                     case _ of
                       "(" -> skipMany stuff <* cutrpar
                       "[" -> skipMany stuff <* cutrsqb
                       "<:" -> skipMany stuff <* cutrsqb
                       "{" -> skipMany stuff <* cutrcur
                       _ -> skipMany stuff
                     |]
               )

-- ** Declarations

-- | This is an expanded form of 'typespecquals' that also accepts
-- storage class specifiers and function specifiers (i.e., there are two:
-- @inline@ and @_Noreturn@).
declspec :: P TT
declspec = undefined

declspecs :: P T
declspecs = undefined

-- | Parse a full C declaration. A single declaration can bring into scope
-- many identifiers, so beware of that. A declaration consists of one
-- or more declarators, and a base type (the declarators modify the base
-- type). Here, each declarator introduces an identifier.
declaration :: P Declaration
declaration = undefined

-- | A shared symbol is allocated here to avoid having to call it
-- when parsing an abstract declarator.
__shared_dummy_sym :: Symbol
__shared_dummy_sym = unsafePerformIO symnew
{-# NOINLINE __shared_dummy_sym #-}

-- | Create a declarator, which introduces an identifier. Associate the
-- identifier with the symbol.
declarator :: Symbol -> P Declarator
declarator = undefined

-- | This is a variant of 'declarator' that does not introduce an
-- identifier into scope, meaning no symbol needs to be provided.
absdeclarator :: P Declarator
absdeclarator = undefined

-- | This will be an environment for 'anydeclarator'. This will have
-- functions that modulate its behavior. A direct declarator also requires
-- a symbol. For an abstract declarator, this can be a dummy symbol.
data AD

-- | This needs to be a very general routine.
anydeclarator :: AD -> P Declarator
anydeclarator = undefined

-- | Braced initializer @{...}@.
bracedinitializer :: P Initializer
bracedinitializer = undefined

-- | An initializer, which is either an @assignment-expression@.
initializer :: P Initializer
initializer = undefined
