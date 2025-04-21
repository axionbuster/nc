{-# LANGUAGE NoMonoLocalBinds #-}
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

-- | Type specifiers and qualifiers, alignment specifiers, and storage
-- class specifiers.
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
    ExAtomic !UQType
  | -- | @typedef@ name. A @typedef@ definition is given as
    -- as a formal storage class specifier, instead. This is
    -- for using a @typedef@ name.
    ExTypedef !Type
  | -- | @typeof@ and @typeof_unqual@
    ExTypeof !TypeofQual !Typeof

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

-- | Is it the same as nothing?
_t_isempty :: T -> Bool
_t_isempty (T a b c d e f) =
  a == 0 && b == 0 && isNothing c && isNothing d && null e && null f

-- | A transformation on 'T'.
newtype TT = TT {aptt :: T -> T}
  deriving (Monoid, Semigroup) via (Endo T)

makeLenses ''T

-- | Throw on duplicate keywords.
_t_ensure_nodup :: T -> P ()
_t_ensure_nodup t
  | t ^. t_dups /= mempty = adhoc' "duplicate found"
  | otherwise = pure ()

-- | Get the 'Qual' out of a 'T'.
_t2qual :: T -> Qual
_t2qual (_t_sqa -> t) =
  foldMap'
    f
    [ (_qu_atomic, SqaAtomic),
      (_qu_const, SqaConst),
      (_qu_restrict, SqaRestrict),
      (_qu_volatile, SqaVolatile)
    ]
 where
  f (l, m) = bool mempty l (t .&. m /= 0)

-- | Extract the 'Type' from a 'T'.
_t2type :: T -> P Type
_t2type t = go
 where
  go =
    let sqa = t ^. t_sqa
        test (ma, me, a)
          | sqa .&. ma /= mempty = Just (me, a)
          | otherwise = Nothing
        tests =
          map test
            $ [ -- test against different "categories." only one must
                -- pass in a valid specification.
                (primmask, "primitives", doprim),
                (recordmask, "struct or union", dorecord),
                (SqaEnum, "enum", doenum),
                (SqaAtomicnewtype, "_Atomic(...)", doatomic),
                (SqaTypedef, "typedef", dotypedef),
                (typeofmask, "typeof or typeof_unqual", dotypeof)
              ]
     in case catMaybes tests of
          [(_, a)] -> a -- the only category.
          [] -> failed -- nothing found.
          cats -> do
            -- must be mutually exclusive, but found not to be.
            let texts = [showsPrec 10 c | (c, _) <- cats]
                t_commas = foldr1 (\f g -> f . (", " ++) . g) texts
                text =
                  ("the categories " ++)
                    . t_commas
                    . (" cannot go together" ++)
            adhoc' $ text "" -- produce string by prepending to "".
  sqa = t ^. t_sqa
  primctormask =
    SqaInt
      .|. SqaBool
      .|. SqaChar
      .|. SqaShort
      .|. SqaLong
      .|. SqaLongLong
      .|. SqaBitInt
      .|. SqaFloat
      .|. SqaDouble
      .|. SqaComplex
      .|. SqaImaginary -- <- we can't handle this.
      .|. SqaD32
      .|. SqaD64
      .|. SqaD128
      .|. SqaVoid
      .|. SqaNullptr
  signmask = SqaSigned .|. SqaUnsigned
  primmask = primctormask .|. signmask
  dosign = case sqa .&. signmask of
    SqaSigned -> pure Signed
    SqaUnsigned -> pure Unsigned
    s
      | s == SqaSigned .|. SqaUnsigned ->
          adhoc' "both signed and unsigned appear"
    _ -> failed
  dosign' = option Signed dosign
  doprim =
    uq2type . UQPrim <$> case sqa .&. primctormask of
      SqaInt -> PInt <$> dosign'
      SqaBool -> pure PBool
      SqaChar -> withOption dosign (pure . PSUChar) (pure PNSChar)
      SqaLong -> PLong <$> dosign'
      SqaFloat -> pure PrimFloat
      SqaDouble -> pure PrimDouble
      SqaLongDouble -> pure PrimLongDouble
      SqaInComplex SqaFloat -> pure PrimCFloat
      SqaInComplex SqaComplex -> pure PrimCDouble
      SqaInComplex SqaLongDouble -> pure PrimCLongDouble
      SqaD32 -> pure PrimD32
      SqaD64 -> pure PrimD64
      SqaD128 -> pure PrimD128
      SqaVoid -> pure PrimVoid
      SqaNullptr -> pure PrimNullptr
      SqaBitInt -> case t ^. t_extra of
        Just (ExBI bsz) -> (`PBitInt` bsz) <$> dosign'
        _ -> pexpect "bit int width"
      _ -> withOption dosign (pure . PInt) do
        pexpect "signed or unsigned"
  recordmask = SqaStruct .|. SqaUnion
  genericdo pr con emsg = case pr $ t ^. t_extra of
    Just y -> pure . uq2type . con $ y
    _ -> pexpect emsg
  pre_record = \case
    Just (ExRec y) -> Just y
    _ -> Nothing
  dorecord = genericdo pre_record UQRecord "a struct or enum definition"
  pre_enum = \case
    Just (ExEnum y) -> Just y
    _ -> Nothing
  doenum = genericdo pre_enum UQEnum "an enum"
  pre_atomic = \case
    Just (ExAtomic y) -> Just y
    _ -> Nothing
  doatomic = genericdo pre_atomic UQAtomic "an _Atomic (...) payload"
  dotypedef = case t ^. t_extra of
    Just (ExTypedef ty) -> pure ty
    _ -> pexpect "a typedef"
  typeofmask = SqaTypeof .|. SqaTypeofUnqual
  pre_typeof = \case
    Just (ExTypeof a b) -> Just (a, b)
    _ -> Nothing
  dotypeof = genericdo pre_typeof (uncurry UQTypeof) do
    "the referent of the typeof(_unqual)"

_t2declspec :: T -> DeclSpec
_t2declspec = undefined

_t2storclass :: T -> Maybe StorageClass
_t2storclass (_t_sqa -> sqa) = case sqa .&. mask of
  SqaAuto -> Just SAuto
  SqaStatic -> Just SStatic
  SqaTypedef -> Just STypedef
  SqaRegister -> Just SRegister
  SqaThreadLocal -> Just SThreadLocal
  SqaConstexpr -> Just SConstExpr
  _ -> Nothing
 where
  mask =
    SqaAuto
      .|. SqaStatic
      .|. SqaTypedef
      .|. SqaThreadLocal
      .|. SqaConstexpr
      .|. SqaRegister

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

-- | Special one for @long double@.
pattern SqaLongDouble :: SQA
pattern SqaLongDouble = 0x4_8000

-- | Wrap it in @_Complex@.
pattern SqaInComplex :: SQA -> SQA
pattern SqaInComplex sqa <- ((.|. SqaComplex) -> sqa)
  where
    SqaInComplex sqa = sqa .|. SqaComplex

-- | Collect \'specifier-qualifier-list\' into a 'Type'. This
-- involves applying 'specqualalign' many times.
specqualaligns :: P Type
specqualaligns = undefined

-- | We use a case-of match instead of a direct string match because
-- this portion is shared between different parsers. We want an efficient
-- trie search which favors the case where every possibility is combined
-- and tested at once.
declspec_real :: SQA -> P TT
declspec_real = \case
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
        $ TT (t_extra .~ Just (ExAtomic (tn ^. ty_base)))
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
            pure $ TT (t_extra .~ Just (ExTypeof qualification t))

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
           "alignas" -> r SqaAlignas
           "_Alignas" -> r SqaAlignas
           "const" -> r SqaConst
           "restrict" -> r SqaRestrict
           "volatile" -> r SqaVolatile
           _ -> r SqaOther
         |]
   )
 where
  r = declspec_real

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
        (pr2type PrimInt)
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
                    $( switch_ws0
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
declspec =
  $( switch_ws1
       [|
         case _ of
           -- \* type specifiers and qualifiers and alignment specifiers
           -- (shared with specqualalign)
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
           "alignas" -> r SqaAlignas
           "_Alignas" -> r SqaAlignas
           "const" -> r SqaConst
           "restrict" -> r SqaRestrict
           "volatile" -> r SqaVolatile
           -- \* function specifiers
           "inline" -> r SqaInline
           "_Noreturn" -> r SqaNoreturn
           -- \* storage class specifiers
           "auto" -> r SqaAuto
           "constexpr" -> r SqaConstexpr
           "extern" -> r SqaExtern
           "register" -> r SqaRegister
           "static" -> r SqaStatic
           "thread_local" -> r SqaThreadLocal
           "_Thread_local" -> r SqaThreadLocal
           "typedef" -> r SqaTypedef
           _ -> r SqaOther
         |]
   )
 where
  r = declspec_real

-- | Parse a list of declaration specifiers. Will be followed by a declarator
-- which transforms the base type returned by 'declspecs'. FIXME: this should
-- also parse optional attribute specifiers that follow them.
declspecs :: P T
declspecs = fmap aptt (chainl (<>) declspec (pure mempty)) <*> pure _t_0

-- | Parse a full C declaration. A single declaration can bring into scope
-- many identifiers, so beware of that. A declaration consists of one
-- or more declarators, and a base type (the declarators modify the base
-- type). Here, each declarator introduces an identifier.
--
-- Note: 'declaration' cannot be used to define a function.
declaration :: P Declaration
declaration = branch static_assert' sadecl do
  attrs <- attrspecs0
  dss <- declspecs
  let dsall = _t2declspec dss & ds_attrs %~ (<> attrs)
  ids <- initdecl dsall `sepBy` comma
  -- attrs  dss*  ids   (type)
  -- yes    yes   yes   (ListDecl)
  -- yes    no    no    (AttrDecl)
  -- no     yes   opt   (ListDecl)
  -- \*: key column
  if _t_isempty dss
    then do
      unless (null attrs) do
        adhoc' "unexpected decleration specifier(s)"
      unless (null ids) do
        adhoc' "unexpected init-declarator(s)"
      AttrDecl attrs <$ semicolon
    else do
      when (null attrs) do
        pexpect "declaration specifier(s)"
      when (null ids) do
        pexpect "init-declarator(s)"
      ListDecl dsall ids <$ semicolon
 where
  sadecl = StaticAssertDecl <$> parsesabody
  initdecl dsall = do
    s <- symnew
    d <- declarator s `pcut_expect` "a declarator"
    i <- optional (equal >> initializer `pcut_expect` "an initializer")
    case (dsall ^. ds_stor, dsall ^. ds_type) of
      (Just st, Just ty) -> pure $ DeclInit (apdecl d ty) st i
      (Just _, _) -> pexpect "type specifier(s)"
      (_, Just _) -> pexpect "a storage class classifier"
      (_, _) -> pexpect "a storage class classifier and type specifier(s)"

-- | Create a declarator, which introduces an identifier. Associate the
-- identifier with the symbol.
declarator :: Symbol -> P Declarator
declarator = anydeclarator ADDirect

-- | This is a variant of 'declarator' that does not introduce an
-- identifier into scope, meaning no symbol needs to be provided.
absdeclarator :: P Declarator
absdeclarator = symnew >>= anydeclarator ADAbstract

-- | 'anydeclarator' configuration.
data AD
  = -- | A direct declarator, which introduces an identifier.
    ADDirect
  | -- | An abstract declarator, which does not introduce an identifier.
    ADAbstract
  | -- | This declarator may or may not introduce an identifier.
    ADAny
  deriving (Eq)

-- | Parse a declarator, using the identifier policy given. It uses 'pureLazy'
-- to avoid redundant evaluation when combining multiple declarator components,
-- since C declarator syntax is right-associative and can involve deeply nested
-- type constructions.
anydeclarator :: AD -> Symbol -> P Declarator
anydeclarator pol sym = do
  ptr <- chainr (<>) pointer (pureLazy mempty)
  bas <- basedecl
  arf <- branch lsqb array $ branch lpar function $ pureLazy mempty
  pureLazy $ ptr <> arf <> bas
 where
  pointer :: P Declarator
  pointer =
    star >> flip pcut_expect "pointer" do
      attrs <- attrspecs0
      quals <- qualifiers
      -- wrap the type in a pointer
      pureLazy $ Declarator \ty ->
        Type' (UQPointer attrs ty) quals
  qualifiers :: P Qual
  qualifiers = do
    f <- chainl (<>) (pure mempty) qualifier
    let g = aptt f _t_0
    _t_ensure_nodup g $> _t2qual g
   where
    qualifier =
      $( switch_ws1
           [|
             case _ of
               "const" -> r SqaConst
               "restrict" -> r SqaRestrict
               "volatile" -> r SqaVolatile
               "atomic" -> r SqaAtomic
             |]
       )
    r = declspec_real
  basedecl :: P Declarator
  basedecl = do
    -- identifier depending on the mode, and then qualifiers.
    when (pol == ADDirect) do
      -- direct declarator: identifier required.
      identifier_def >>= symgivegeneralname sym
    when (pol == ADAny) do
      -- only give identifier if it can be parsed.
      withOption identifier_def (symgivegeneralname sym) (pure ())
    -- (abstract declarator: identifier must not be present.)
    quals <- qualifiers
    pureLazy $ Declarator $ over ty_qual (<> quals)
  -- body of an array. thus, past [. we manually close ] because we need
  -- to handle optional attributes that go after it.
  array :: P Declarator
  array = flip pcut_illegal "array declarator" do
    -- static_position:
    --  0 . . . does not occur
    --  1 . . . head (before qualifiers)
    --  2 . . . after qualifiers
    (static_position :: Word8, quals) <- do
      branch static' ((1,) <$> qualifiers) do
        withOption
          qualifiers
          ( \qs -> do
              s <- isJust <$> optional static'
              pure (bool 0 2 s, qs)
          )
          (pure (0, mempty))
    -- len:
    --    Nothing Nothing . . . no expression.
    --    Just Nothing    . . . given the VLA star (*).
    --    Just (Just _)   . . . an expression giving array size.
    len <- optional $ (Just <$> expr) <|> (Nothing <$ star)
    attrs <- rsqb >> attrspecs0
    let
      -- arrays used in function prototypes will carry
      -- this information. we will include it regardless
      -- of whether it's really in a function prototype.
      paraminfo
        | static_position == 0 && quals == mempty = Nothing
        | otherwise =
            Just
              $ ParamArrayInfo
                ((static_position /= 0) ^. from isarraystatic)
                quals
      finish = pureLazy $ Declarator \ty ->
        Type' (UQArray $ ArrayInfo (join len) ty paraminfo attrs) mempty
    -- allowed combinations:
    --  static_position = 0:
    --    if direct,
    --      optional qualifiers.
    --      optional expression or the VLA star.
    --    if abstract,
    --      if VLA star (*) parses instead of an expression,
    --        no qualifiers.
    --        the vla star.
    --      else,
    --        optional qualifiers.
    --        optional expression.
    --  static_position = 1:
    --    optional qualifiers.
    --    expression.
    --  static_position = 2:
    --    at least one qualifier in a list.
    --    ("static").
    --    expression.
    --  (no other combinations).
    case static_position of
      0 -> case pol of
        ADAbstract -> case len of
          Just Nothing | quals == mempty -> finish
          Just Nothing -> adhoc' "qualifiers unexpected"
          _ -> finish
        _ -> finish
      1 -> finish
      2 | quals == mempty -> pexpect "at least one qualifier"
      2 -> finish
      _ -> error "this is impossible"
  -- body of a function prototype. thus, past (. we manually close the )
  -- to parse the optional attribute specifiers that may come after it.
  function :: P Declarator
  function = flip pcut_illegal "function declarator" do
    params <- flip sepBy comma param
    isvar <-
      isJust <$> optional do
        unless (null params) comma
        tripledot
    attrs <- rpar >> attrspecs0
    pureLazy $ Declarator \ty ->
      uq2type
        $ UQFunc
        $ FuncInfo ty params (isvar ^. from isvariadic) attrs
   where
    param = do
      attrs <- attrspecs0
      base <- declspecs >>= _t2type
      sym <- symnew
      decl <- option mempty (anydeclarator ADAny sym)
      let ty = apdecl decl base
      pure $ Param attrs ty sym

-- | A braced initializer @{...}@.
bracedinitializer :: P Initializer
bracedinitializer =
  incur
    $ bracedinitializer_body
    `pcut_expect` "braced initializer"

-- | The body of a braced initializer @{...}@. May designate an empty braced
-- initializer.
bracedinitializer_body :: P Initializer
bracedinitializer_body = InitBraced <$> clause `sepEndBy` comma
 where
  clause = InitItem <$> designators <*> initializer
  designators = option [] $ some designator <* (equal `pcut_expect` "=")
  designator =
    $( switch_ws0
         [|
           case _ of
             "[" -> oncur <* cutrcur
             "<:" -> oncur <* cutrcur
             "." -> DesignatorMember <$> identifier
           |]
     )
  oncur = DesignatorIndex <$> constexpr

-- | An initializer, which is either an @assignment-expression@,
-- or a 'bracedinitializer'.
initializer :: P Initializer
initializer =
  branch_incur bracedinitializer_body
    $ InitExpr
    <$> assignexpr
