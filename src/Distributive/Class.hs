{-# language MagicHash #-}
module Distributive.Class (module Distributive.Class, module X) where
import Applicative.Class as X
import FoldMap_.Class -- TODO: factor into applicative?
import Type.I
import Type.K
import Coerce
import Prelude (($)) -- TODO: reexport

class Applicative t => Distributive t where
  {-# minimal distribute | collect | zipF #-}
  distribute :: Map f => f (t a) -> t (f a)
  distribute = collect (\x -> x)
  -- aka cotraverse
  zipF :: Map f => (f a -> b) -> f (t a) -> t b
  zipF f = \fta -> map f (distribute fta)
  collect :: Map f => (a -> t b) -> f a -> t (f b)
  collect f a  = zipF (\x -> x) (map f a)
  -- TODO: is collect (\x -> x) === cotraverse (\x -> x)

instance Distributive ((->) x) where
  collect axb fa = \x -> (\a -> axb a x) `map` fa
  distribute fxa = \x -> (\f -> f x) `map` fxa

instance Distributive I where distribute a = I (map fold_ a)
