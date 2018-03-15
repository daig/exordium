module Apply (module Apply, module X) where
import Map as X
import Num.Add
import {-# source #-} K
import {-# source #-} I
import {-# source #-} E

-- | (f |$(<)$| g) |$| w = f |$| (g |$| w)
class Map f => Apply f where
  ap :: f (a -> b) -> f a -> f b


liftA2 :: Apply f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f fa = fa |$ f

(|$|) :: Apply f => f (a -> b) -> f a -> f b
(|$|) = ap
(|!|) :: Apply f => f a -> f b -> f a
(|!|) = constAp
(|$) :: Apply f => f a -> (a -> b -> c) -> f b -> f c
fa |$ f = (map f fa |$|)
($|) :: (f b -> f c) -> f b -> f c
f $| fb = f fb

apply_add :: (Apply f, Add a) => f a -> f a -> f a
apply_add = liftA2 add

constAp :: Apply f => f a -> f b -> f a
fa `constAp` fb = (\a _ -> a) `map` fa `ap` fb

instance Apply I where I f `ap` I a = I (f a)
instance Apply ((->) x) where f `ap` g = \x -> f x (g x)
instance Apply [] where fs `ap` as = [f a | f <- fs, a <- as]
instance Add a => Apply (K a) where K a `ap` K b = K (a `add` b)

