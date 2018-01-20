module Utils.Apply (module Utils.Apply, module X) where
import Class.Apply as X
import Class.Plus as X

liftA2 :: Apply f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f fa = fa |$ f

(|$|) = ap
(|!|) = constAp
(|$) :: Apply f => f a -> (a -> b -> c) -> f b -> f c
fa |$ f = (map f fa |$|)
($|) :: (f b -> f c) -> f b -> f c
f $| fb = f fb

apply_plus :: (Apply f, Plus a) => f a -> f a -> f a
apply_plus = liftA2 (+)
