module Utils.Apply (module Utils.Apply, module X) where
import Apply.Class as X
import Plus.Class as X

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

apply_plus :: (Apply f, Plus a) => f a -> f a -> f a
apply_plus = liftA2 plus
