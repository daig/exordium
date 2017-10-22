module Distributive
  (Distributive(..)
  ,zipWith, mapDefault
  ,module X) where
import Map as X
import I

  -- O < map distribute < collect f = collect (O < f)
  -- distribute < distribute = id
class Map t => Distributive t where
  {-# minimal distribute | collect | zipFWith #-}
  distribute :: Map f => f (t a) -> t (f a)
  distribute = collect (\x -> x)
  -- aka cotraverse
  zipFWith :: Map f => (f a -> b) -> f (t a) -> t b
  zipFWith f = \fta -> map f (distribute fta)
  collect :: Map f => (a -> t b) -> f a -> t (f b)
  collect f a  = zipFWith (\x -> x) (map f a)
  -- TODO: is collect (\x -> x) === cotraverse (\x -> x)

-- TODO: merge into data family
data V2 a = V2 ~a ~a
instance Map V2 where map f (V2 a b) = V2 (f a) (f b)
zipWith :: Distributive t => (a -> a -> b) -> t a -> t a -> t b
zipWith f t t' = zipFWith (\(V2 a b) -> f a b) (V2 t t')

mapDefault :: Distributive t => (a -> b) -> t a -> t b
mapDefault f ta = case collect (\x -> I (f x)) ta of I tb -> tb


instance Distributive ((->) x) where
  collect axb fa = \x -> map (\a -> axb a x) fa

instance Distributive I where distribute a = I (map fold_ a)
