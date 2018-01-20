module Optic where
import Type.E
import Class.Map
{-import Curry-}
import Flip
import O

type Optical p q f s a b t = p a (f b) -> q s (f t)

-- | Optic = Optical (->) (->)
type Optic f s a b t = (a -> f b) -> s -> f t

-- | Optic' f s a = Optic f s a a s
type Optic' f s a = (a -> f a) -> s -> f s


{-(~*~) :: Optic (Curry' f b') s t a b -> Optic (Curry f t) s' t' a' b' -> Optic f (s, s') (t, t') (a, a') (b, b')  -}
{-(l1 ~*~ l2) f (a1, a2) = getCurry $ l2 @$ a2 $ \b2 -> Curry $ getCurry'  $ l1 @$ a1 $ \b1 -> Curry' $ f (b1,b2)-}
{-(~|~) :: Map f => Optic f s t a b -> Optic f s' t' a b -> Optic f (E s s') (E t t') a b-}
{-(l ~|~ _) f (L s)   = L $@ l f s-}
{-(_ ~|~ r) f (R s') = R $@ r f s'-}
