module X.Functor.IEMap (IEMap(..), module X) where
import X.Functor.HMap as X
import X.Functor.IMap as X

-- | iemap f . iemap g = iemap (\i -> f i <=< g i)
--   iemap (\_ -> R . f) = map f
class (IMap i f, HMap e f) => IEMap i e f where
  iemap :: (i -> a -> E e b) -> f a -> f b
  iemap iaeb fa = throwing (imap iaeb fa)
  update :: Eq' i => (a -> E e a) -> i -> f a -> f a
  update f i = iemap (\i' a -> if i `eq` i' then f a else R a)
  alter :: (i -> E e a -> E e b) -> f a -> f b
  alter ieaeb fa = iemap ieaeb (catching fa)
