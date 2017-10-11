module Lens where
import Lens.Type
import Sum
import Map
import Curry
import Flip
import O

(~*~) :: Optic (Curry' f b') s t a b -> Optic (Curry f t) s' t' a' b' -> Optic f (s, s') (t, t') (a, a') (b, b')  
(l1 ~*~ l2) f (a1, a2) = getCurry $ l2 ?? a2 $ \b2 -> Curry $ getCurry'  $ l1 ?? a1 $ \b1 -> Curry' $ f (b1,b2)
(~|~) :: Map f => Optic f s t a b -> Optic f s' t' a b -> Optic f (E s s') (E t t') a b
(l ~|~ _) f (L s)   = L `map` l f s
(_ ~|~ r) f (R s') = R `map` r f s'

fab ?? a = map (\f -> f a) fab
infixl 1 ??
f $ a = f a
infixr 0 $
