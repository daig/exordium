{-# language MagicHash #-}
module X.Cast.Internal where
import X.Type.Int.I
import X.Arrow.Promap
import X.Arrow.Traversed
import X.Stock.Enum
import GHC.Enum (Bounded(..))
import qualified Prelude as P

-- TODO: broken if maxBound outside Int's maxBound (ex: for Word64).
-- Fix after making a good toInteger class in Num
_enum :: forall a p. (Bounded a, Enum a, Traversed' p) => p a a -> p Int Int
_enum = prism down up where
  down i = if i `gt#` fromEnum (maxBound @a) `max#` i `lt#` fromEnum (minBound @a)
    then L i else R (toEnum i)
  up = fromEnum

_enum_ :: (Enum a, Promap p) => p a a -> p Int Int
_enum_ = promap toEnum fromEnum
