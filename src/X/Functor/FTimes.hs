module X.Functor.FTimes (FTimes(..),module X) where
import X.Functor.Remap as X
import X.Num.Add
import X.Type.K
import X.Type.I
import X.Type.IO
import X.Data.E
import qualified Control.Applicative as P

-- | remap (f *** x) (g *** y) (ftimes a b) = remap f g a `ftimes` remap x y b
-- | remap f g a `ftimes` remap x y b = remap (O

--   remap (f . g) (x . y)
-- | (f |$(<)$| g) |$| w = f |$| (g |$| w)
class Remap f => FTimes f where ftimes :: f a -> f b -> f (a,b)

instance FTimes I where I a `ftimes` I b = I (a,b)
instance FTimes ((->) x) where f `ftimes` g = \x -> (f x,g x)
instance FTimes [] where as `ftimes` bs = [(a,b) | a <- as, b <- bs]
instance Add a => FTimes (K a) where K a `ftimes` K b = K (a `add` b)
instance Add a => FTimes ((,) a) where (a,x) `ftimes` (b,y) = (add a b, (x,y))

instance FTimes IO where ftimes = P.liftA2 (,)
instance FTimes (E x) where
  ftimes (L x) _ = L x
  ftimes (R a) (R b) = R (a,b)
  ftimes _ (L x) = L x
