module X.Cast.Coerce where
import Data.Coerce (Coercible)
import qualified Data.Coerce as C
{-import X.Arrow.Promap-}

-- | Representational type equality. Contrast with nominal equality `~`.
-- Conversion may not preserve semantics of nominal typeclass instances.
type (#=#) = Coercible
coerce :: b #=# a => a -> b
coerce = C.coerce

{-_coerce_ :: forall a s p. (Promap p, s #=# a, a #=# s ) => p a a -> p s s-}
{-_coerce_ = promap coerce coerce-}

coerceF :: forall g f a. f a #=# g a => f a -> g a
coerceF = C.coerce

wrap :: forall f a. a #=# f a => a -> f a
wrap = C.coerce

unwrap :: forall a f. f a #=# a => f a -> a
unwrap = C.coerce
