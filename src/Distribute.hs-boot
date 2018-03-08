module Distribute (module Distribute, module X) where
import Applicative as X

class Applicative t => Distribute t where
  {-# minimal distribute | collect | zipF #-}
  distribute :: Map f => f (t a) -> t (f a)
  distribute = collect (\x -> x)
  -- aka cotraverse
  zipF :: Map f => (f a -> b) -> f (t a) -> t b
  zipF f = \fta -> map f (distribute fta)
  collect :: Map f => (a -> t b) -> f a -> t (f b)
  collect f a  = zipF (\x -> x) (map f a)
