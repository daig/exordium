module Cast.Internal where
import Type.Int.I
import Map.Pro
import Traversed
import Enum
import GHC.Enum (Bounded(..))
import qualified Prelude as P

-- TODO: broken if maxBound outside Int's maxBound (ex: for Word64).
-- Fix after making a good toInteger class in Num
_enum :: forall a p. (Bounded a, Enum a, Traversed' p) => p a a -> p Int Int
_enum = prism down up where
  down i = if i P.> fromEnum (maxBound @a) || i P.< fromEnum (minBound @a)
    then L i else R (toEnum i)
  up = fromEnum

_enum_ :: (Enum a, Promap p) => p a a -> p Int Int
_enum_ = promap toEnum fromEnum
