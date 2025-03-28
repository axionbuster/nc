{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Language.NC.Parse.Decl where

import Language.NC.Experiment.Types
import Language.NC.Internal.Prelude
import Language.NC.Lex2
import Language.NC.Parse.Expr

-- | A declaration is a statement that introduces a new name
--   or identifier into the program. It can be a type declaration,
--   a type alias declaration, or a data declaration.
data Decl
  = -- | @struct@, @union@, @enum@, or our new @union struct@
    TypeDecl Type
  | -- | @typedef@
    TypeAliasDecl Name Type
  | -- | Object or function declaration
    DataDecl
      { -- | storage class specifier
        declstor :: Maybe Storage,
        -- | qualified type
        decltype :: Type,
        -- | exactly one name or unique identifier
        --   this is even for anonymous declarations;
        --   we give them internal unique identifiers
        declname :: Name,
        -- | optional initializer
        declinit :: Maybe Expr
      }

_decltype :: Lens' Decl Type
_decltype f (TypeDecl x) = TypeDecl <$> f x
_decltype f (TypeAliasDecl n x) = TypeAliasDecl n <$> f x
_decltype f (DataDecl a b c d) = (\x -> DataDecl a x c d) <$> f b

_typeattr :: Lens' Type [Attribute]
_typeattr f t = (\x -> t {typeattr = x}) <$> f (typeattr t)

_decltypeattr :: Lens' Decl [Attribute]
_decltypeattr = _decltype . _typeattr

-- now this is a proper monad to help us cope with the "spiral rule" in C
-- for parsing declarations. it allows us to separate parsing the
-- "type" part from the rest of the declaration. the "type" itself may
-- need to change as well, hence it returns (Type, a) instead of @a@.
type RWBind a =
  -- | left side of a declarator
  Type ->
  -- | changed type and resulting value
  (Type, a)

-- a poor man's monad declaration because i'm too lazy to actually
-- implement a proper monad for this. this is enough for now.

rwpure :: a -> RWBind a
rwpure x = (,x)

rwmap :: (a -> b) -> RWBind a -> RWBind b
rwmap f a = \typ ->
  let (newtyp, x) = a typ
   in (newtyp, f x)

rwmapr :: b -> RWBind a -> RWBind b
rwmapr y a = \typ ->
  let (newtyp, _) = a typ
   in (newtyp, y)

rwbind :: RWBind a -> (a -> RWBind b) -> RWBind b
rwbind a f = \typ ->
  let (newtyp, x) = a typ
      (newtyp', y) = f x newtyp
   in (newtyp', y)

rw_ap :: RWBind (a -> b) -> RWBind a -> RWBind b
rw_ap a b = \typ ->
  let (newtyp, f) = a typ
      (newtyp', x) = b newtyp
   in (newtyp', f x)

(>@=@>) :: RWBind a -> (a -> RWBind b) -> RWBind b
(>@=@>) = rwbind

infixl 1 >@=@>

-- | Is it a function declaration?
declisfunc :: Decl -> Bool
declisfunc DataDecl {decltype = Type {typetype}} =
  catdata typetype == CatFunction
declisfunc _ = False

-- this is the name part of a declaration...
declarator =
  -- if we are parsing a pointer, the pointer parser will
  -- accept and return a function that wraps the direct
  -- declaration into the pointer declaration. otherwise,
  -- we just parse the direct declaration.
  withOption (lex pointer) (<$> direct) direct

pointer :: Parser (RWBind (Type -> Type))
pointer =
  -- this one is a bit tricky. it returns a single pointer,
  -- but it can be arbitrarily nested. the return value is
  -- a single function, but it may be chained using (.).
  chainr (.) (pure id) do
    lx0 star
    attrs <- some $ lx attrspec
    quals <- some $ lx qualspec
    pure \basetype ->
      ( basetype,
        Type
          { typequal = quals,
            typeattr = attrs,
            typetype = PPointer $ CPointer basetype
          }
      )

attrspec = error "parse an attribute specifier."

qualspec = error "parse a type qualifier."

idr = sbsOf identifier

-- the so-called "direct declarator" in the official C grammar.
-- the name part of a declarator (mostly speaking). well, because
-- of the difficult spiral rule, it's a bit more complicated than just
-- a name...
direct :: Parser (RWBind Name)
direct =
  choice
    [ -- simple: say a name has a type. that's it.
      -- this is the base case of the recursion.
      apat $ lex idr,
      -- parentheses (grouping). just changes the
      -- precedence of the expression. this allows us to
      -- parse expressions like (int (*)[5]) and (int (*)) correctly.
      -- also, no attributes.
      inpar declarator,
      -- function
      apat directfunc,
      -- array
      apat directarray
    ]
  where
    apat f = do
      attrs <- many $ lex attrspec
      case attrs of
        [] -> f
        _ -> over _typeattr (attrs ++) <$> f

directfunc = error "parse a function declaration."

directarray = error "parse an array declaration."
