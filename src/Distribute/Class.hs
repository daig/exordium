{-# language MagicHash #-}
module Distribute.Class (module Distribute.Class, module X) where
import Applicative.Class as X

class Applicative t => Distribute t where
  {-# minimal distribute | collect | zipF #-}
  distribute :: Map f => f (t a) -> t (f a)
  distribute = collect (\x -> x)
  -- aka cotraverse
  zipF :: Map f => (f a -> b) -> f (t a) -> t b
  zipF f = \fta -> map f (distribute fta)
  collect :: Map f => (a -> t b) -> f a -> t (f b)
  collect f a  = zipF (\x -> x) (map f a)

instance Distribute ((->) x) where
  collect axb fa = \x -> (\a -> axb a x) `map` fa
  distribute fxa = \x -> (\f -> f x) `map` fxa