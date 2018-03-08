module Apply.Class (Apply(..),module X) where
import Map.Class as X
import Plus.Class as X
import {-# source #-} K


-- | (f |$(<)$| g) |$| w = f |$| (g |$| w)
class Map f => Apply f where
  ap :: f (a -> b) -> f a -> f b

instance Apply ((->) x) where f `ap` g = \x -> f x (g x)
instance Apply [] where fs `ap` as = [f a | f <- fs, a <- as]
instance Plus a => Apply (K a) where K a `ap` K b = K (a `plus` b)
