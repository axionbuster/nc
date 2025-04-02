module Language.NC.Internal.Error where

import Control.Exception
import Data.String
import Data.Text (Text)
import Data.Word
import FlatParse.Stateful (Span)
import Text.Printf (printf)
import Prelude (Eq, Ord, Show (..), mempty)

-- | Severity of a diagnostic message
data Severity
  = -- | Error stops compilation
    SeverityError
  | -- | Warning continues compilation
    SeverityWarning
  | -- | Note is informational only
    SeverityNote
  deriving (Eq, Show, Ord)

-- | Function that determines the final severity for a given error and default severity
type SeverityPolicy = Error -> Severity -> Severity

-- | An error, warning, or message of any kind.
--
-- Not all \"errors\" may be errors.
data Error
  = -- | Miscellaneous programming error
    BasicError String
  | -- | Bad primitive type
    PrimTypeBadError PrimTypeBadWhy
  | -- | Literal problem
    LiteralBadError LiteralBadWhy
  | -- | Unexpected end of file
    UnexpectedEOFError
  | -- | Internal error
    InternalError String
  | -- | Symbol redefinition
    SymbolRedefinitionError SymbolRedefinitionWhy
  deriving (Eq, Show)

-- | Reasons for symbol redefinition errors
data SymbolRedefinitionWhy
  = -- | Symbol already defined in current scope
    AlreadyDefinedInScope
  | -- | Type mismatch in redefinition
    TypeMismatch
  deriving (Eq, Show)

-- | Why a primitive type couldn't be parsed, or is incorrect.
data PrimTypeBadWhy
  = -- | Too many specifiers of the same type (e.g. "multiple sign specifiers")
    TooManySpecifiers String
  | -- | Incompatible type combination (e.g. \"mixing void with float\")
    --
    --     If the list of \"other types\" is empty, then say,
    --     \"mixing [void] with other types\" (replace [void] with the type).
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

data LiteralBadWhy
  = -- | Incorrect integer suffix combo.
    IncorrectIntSuffix
  | -- | Literal too large to fit.
    LiteralTooLarge
  | -- | Unsupported character encoding.
    UnsupportedEncoding
  | -- | Invalid character in a string or character literal.
    BadChar
  deriving (Eq)

instance Show LiteralBadWhy where
  show = \case
    IncorrectIntSuffix -> "incorrect integer suffix"
    LiteralTooLarge -> "literal is too large to fit"
    UnsupportedEncoding -> "unsupported character encoding"
    BadChar -> "invalid character in a string or character literal"

instance Exception Error

instance IsString Error where
  fromString = BasicError

-- | An error annotated with span, severity, and optional hints.
data AnnotatedError = AnnotatedError
  { aeerr :: !Error,
    aespn :: !Span,
    aesev :: !Severity,
    aerel :: ![RelatedInfo]
  }
  deriving (Eq, Show)

-- | Create a new empty annotated error with the given span and severity.
aenew :: Error -> Span -> Severity -> AnnotatedError
aenew e s sev = AnnotatedError e s sev mempty

-- | Create a new error with Error severity.
aenewerror :: Error -> Span -> AnnotatedError
aenewerror e s = aenew e s SeverityError

-- | Create a new warning with Warning severity.
aenewwarning :: Error -> Span -> AnnotatedError
aenewwarning e s = aenew e s SeverityWarning

-- | Default severity policy (keeps original severity).
defaultsevpolicy :: SeverityPolicy
defaultsevpolicy _ s = s

-- | Hints and other related info.
data RelatedInfo = RelatedInfo
  { infospan :: Span,
    infomesg :: Text
  }
  deriving (Eq, Show)
