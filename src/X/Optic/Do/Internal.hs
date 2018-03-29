module X.Optic.Do.Internal where
import X.Functor.Applicative 

newtype FK f a b = FK {runFK :: f a}
instance  Map (FK f a) where map _ (FK fa) = FK fa
instance Apply f => Apply (FK f a) where
  ap (FK fa) (FK fb) = FK ((\_ b -> b) `map` fa `ap` fb)
instance (Pure f, Zero a) => Pure (FK f a) where pure !_ = FK (pure zero)
instance (Applicative f, Zero a) => Applicative (FK f a)
