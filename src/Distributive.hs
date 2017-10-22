module Distributive (Distributive(..), module X) where
import Map as X
import I

  -- O < map distribute < collect f = collect (O < f)
  -- distribute < distribute = id
class Map t => Distributive t where
  {-# minimal distribute | collect | cotraverse #-}
  distribute :: Map f => f (t a) -> t (f a)
  distribute = collect (\x -> x)
  -- aka cotraverse
  zipFWith :: Map f => (f a -> b) -> f (t a) -> t b
  zipFWith f = \fta -> map f (distribute fta)
  collect :: Map f => (a -> t b) -> f a -> t (f b)
  collect f a  = zipFWith (\x -> x) (map f a)
  -- TODO: is collect (\x -> x) === cotraverse (\x -> x)

mapDefault :: Distributive t => (a -> b) -> t a -> t b
mapDefault f ta = case collect (\x -> I (f x)) ta of I tb -> tb


instance Distributive ((->) x) where
  collect axb fa = \x -> map (\a -> axb a x) fa

instance Distributive I where distribute a = I (map fold_ a)
