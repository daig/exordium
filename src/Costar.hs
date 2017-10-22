module Costar (Costar(..),module X) where
import Closed as X
import Map as X

newtype Costar f a b = Costar (f a -> b)

instance Map f => Closed (Costar f) where
  closed (Costar fab) = Costar (\fxa x -> fab (map (\f -> f x) fxa))
instance Map f => Dimap (Costar f) where
  dimap f g (Costar fab) = Costar (dimap (map f) g fab)
instance Map f => ComapL (Costar f) where comapl = premap
instance MapR (Costar f) where mapr g (Costar fab) = Costar (\fa -> g (fab fa))
instance Map (Costar f a) where map = mapr
