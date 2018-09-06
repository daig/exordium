module X.Functor.Errors (module X.Functor.Errors, module X) where
import X.Functor.HTraverse as X
import X.Functor.EMap as X
import X.Num.Add0 as X
import X.Functor.Applicative as X

class EFold e f where
  efoldmap :: Add0 m => (a -> E e m) -> f a -> m

  
class ETraverse e t where
  etraverse :: (Applicative f,Throw e f) => (a -> f b) -> t a -> f (t b)

-- | emap (\_ -> L e) = \_ -> throw e
-- | hmap eb _ . throw = pure . eb
class EMap e f => Throw e f where throw :: e -> f a
