module Language.NC.Internal.Error where

import Control.Exception
import Data.String
import Data.Text (Text)
import Data.Word (Word8)
import FlatParse.Stateful (Span)
import Text.Printf (printf)
import Prelude (Eq, Show (..), mempty)

-- | An error, warning, or message of any kind.
--
-- Not all \"errors\" may be errors.
data Error
  = -- | Miscellaneous programming error
    BasicError String
  | -- | Bad primitive type
    PrimTypeBadError PrimTypeBadWhy
  | -- | Unexpected end of file
    UnexpectedEOFError
  | -- | Internal error
    InternalError String
  deriving (Eq, Show)

-- | Why a primitive type couldn't be parsed, or is incorrect.
data PrimTypeBadWhy
  = -- | Too many specifiers of the same type (e.g. "multiple sign specifiers")
    TooManySpecifiers String
  | -- | Incompatible type combination (e.g. \"mixing void with float\")
    --
    -- If the list of \"other types\" is empty, then say,
    -- \"mixing [void] with other types\" (replace [void] with the type).
    IncompatibleTypes String [String]
  | -- | Unrecognized or invalid _BitInt width
    InvalidBitIntWidth Word8
  | -- | Invalid _BitInt width due to overflow or bad formatting
    InvalidBitIntWidthOverflowOrBadFormat
  | -- | Invalid _Decimal bits specification
    InvalidDecimalBits Word8
  | -- | Giving signedness to non-integral type
    InvalidSignedness String
  | -- | Constructing a decimal complex type
    InvalidDecimalComplex
  | -- | Empty or unsupported type specification
    InvalidTypeSpec String
  deriving (Eq)

instance Show PrimTypeBadWhy where
  show = \case
    TooManySpecifiers s -> printf "too many %s specifiers" s
    IncompatibleTypes s [] -> printf "mixing %s with other types" s
    IncompatibleTypes s ts -> printf "mixing %s with %s" s (show ts)
    InvalidBitIntWidth w -> printf "invalid _BitInt width %v" w
    InvalidBitIntWidthOverflowOrBadFormat ->
      "invalid _BitInt width due to overflow or bad formatting"
    InvalidDecimalBits b -> printf "invalid _Decimal bits %v" b
    InvalidSignedness t ->
      printf "giving signedness to non-integral type %s" t
    InvalidDecimalComplex -> "constructing a decimal complex type"
    InvalidTypeSpec t -> printf "empty or unsupported type specification %s" t

instance Exception Error

instance IsString Error where
  fromString = BasicError

-- | An error annotated with span and optional hints.
data AnnotatedError = AnnotatedError
  { aeerr :: Error,
    aespn :: Span,
    aerel :: [RelatedInfo]
  }
  deriving (Eq, Show)

-- | Create a new empty annotated error with the given span.
aenew :: Error -> Span -> AnnotatedError
aenew e s = AnnotatedError e s mempty

-- | Hints and other related info.
data RelatedInfo = RelatedInfo
  { infospan :: Span,
    infomesg :: Text
  }
  deriving (Eq, Show)
