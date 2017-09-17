module Fun
  ((<)
  ,type (=#),coerce
  ) where
import qualified Data.Coerce as C

(<) :: (x -> b) -> (a -> x) -> a -> b
(f < g) a = f (g a)


-- | Representational type equality. Contrast with nominal equality `~`
type (=#) = C.Coercible
coerce :: forall b a. a =# b => a -> b
coerce = C.coerce
