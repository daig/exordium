module X.Cast.Coerce where
import Data.Coerce (Coercible)
import qualified Data.Coerce as C
import qualified Unsafe.Coerce as C
{-import X.Arrow.Promap-}

-- | Representational type equality. Contrast with nominal equality `~`.
-- Conversion may not preserve semantics of nominal typeclass instances.
type (#=#) = Coercible
coerce :: b #=# a => a -> b
coerce = C.coerce

coerceF :: g a #=# f a => f a -> g a
coerceF = C.coerce

wrap :: f a #=# a => a -> f a
wrap = C.coerce

unwrap :: a #=# f a => f a -> a
unwrap = C.coerce
