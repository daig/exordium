module X.Functor.Apply (module X.Functor.Apply, module X) where
import X.Functor.Map as X
import X.Num.Add
import X.Type.K
import X.Type.I
import X.Type.IO
import X.Data.E
import X.Functor.FTimes as X
import X.Data.Maybe
import qualified Prelude as P

-- | (f |$(<)$| g) |$| w = f |$| (g |$| w)
class (FTimes f, Map f) => Apply f where
  ap :: f (a -> b) -> f a -> f b
  ap fab fa = (\(f,a) -> f a) `map` ftimes fab fa


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

ap_ftimes a b = map (,) a `ap` b

constAp :: Apply f => f a -> f b -> f a
fa `constAp` fb = (\a _ -> a) `map` fa `ap` fb

instance Apply I where ap = coerce

instance Apply ((->) x) where
  f `ap` g = \x -> f x (g x)
instance Apply [] where
  {-# INLINE ap #-}
  fs `ap` as = [f a | f <- fs, a <- as]
instance Add a => Apply (K a) where K a `ap` K b = K (a `add` b)
instance Add a => Apply ((,) a) where (a,f) `ap` (b,x) = (add a b, f x)

instance Apply IO where ap = (P.<*>)
instance Apply (E x) where
  ap (L x) _ = L x
  ap (R f) (R a) = R (f a)
  ap _ (L x) = L x

instance Apply Maybe where
  ap (Just f) (Just a) = Just (f a)
  ap _ _ = Nothing
