module X.Functor.Zip (module X.Functor.Zip, module X) where
import X.Functor.Applicative as X

class Applicative t => Zip t where
  {-# minimal distribute | collect | zip #-}
  distribute :: Map f => f (t a) -> t (f a)
  distribute = collect (\x -> x)
  -- aka cotraverse
  zip :: Map f => (f a -> b) -> f (t a) -> t b
  zip f = \fta -> map f (distribute fta)
  collect :: Map f => (a -> t b) -> f a -> t (f b)
  collect f a  = zip (\x -> x) (map f a)
