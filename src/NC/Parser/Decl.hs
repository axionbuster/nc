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

import NC.Internal.Prelude1

-- * Internal

-- | Type specifiers and qualifiers and alignment specifiers.
newtype SQA = SQA Word64
  -- Num instance allows integer literals to be used, but it is
  -- otherwise nonsense.
  deriving (Eq, Show, Bits, Num)
  deriving (Monoid, Semigroup) via (Ior SQA)

-- | Extra data for 'SpecQuals'
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

-- | @specifier-qualifier-list@ information.
data SpecQuals = SpecQuals
  { -- | Final specifier or qualifier.
    _rt_saq :: {-# UNPACK #-} !SQA,
    -- | This specifier or qualifier was repeated too often.
    _rt_dups :: {-# UNPACK #-} !SQA,
    -- | Any explicit @alignof@ directive.
    _rt_align :: !(Maybe Alignment),
    -- | Extra data.
    _rt_extra :: !(Maybe Extra),
    -- | Optional attributes can go at the end of a @specifier-qualifier-list@.
    _rt_attrs :: ![Attribute]
  }

-- | Initial value for 'SpecQuals'
_rt_0 :: SpecQuals
_rt_0 = SpecQuals 0 0 Nothing Nothing []

-- | A transformation on 'SpecQuals'.
newtype TRT = TRT {aptrt :: SpecQuals -> SpecQuals}
  deriving (Monoid, Semigroup) via (Endo SpecQuals)

makeLenses ''SpecQuals

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

sqa_typeof_unqual, sqa_atomicnewtype :: Lens' SQA Bool
sqa_typeof_unqual = __bool 0x10_0000_0000
sqa_atomicnewtype = __bool 0x20_0000_0000

-- * Exported

-- ** Types

typename = undefined

-- ** Attributes

attributes = undefined

-- ** Declarations

declspec = undefined

declaration = undefined

declarator = undefined

absdeclarator = undefined

anydeclarator = undefined

bracedinitializer = undefined

initializer = undefined
