module Class.Apply (Apply(..),module X) where
import Class.Map as X
import Utils.K
import Utils.Fun
import Utils.List
import Class.Plus as X


-- | (f |$(<)$| g) |$| w = f |$| (g |$| w)
class Map f => Apply f where
  {-# minimal ap #-}
  ap :: f (a -> b) -> f a -> f b
  constAp :: f a -> f b -> f a
  fa `constAp` fb = (\a _ -> a) `map` fa `ap` fb


instance Apply ((->) x) where f `ap` g = \x -> f x (g x)
instance Apply [] where fs `ap` as = [f a | f <- fs, a <- as]
instance Plus a => Apply (K a) where K a `ap` K b = K (a `plus` b)
