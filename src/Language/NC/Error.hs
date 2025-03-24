module Language.NC.Error where

import Control.Exception
import Data.String
import Data.Text (Text)
import FlatParse.Stateful (Span)
import Prelude (Eq, Show, mempty)

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
  = -- | @(un)signed long double@
    BecauseSignInLongDouble
  | -- | Type cannot be given signedness
    BecauseSignNotMeaningful
  | -- | Weird suffix
    BecauseWeirdSuffix
  deriving (Eq, Show)

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
