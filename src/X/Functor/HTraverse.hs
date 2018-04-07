module X.Functor.HTraverse where
import X.Type.I
import X.Type.K
import X.Functor.HFold as X
import X.Functor.HMap as X
import X.Functor.Applicative as X

class (HMap e t, HFold e t) => HTraverse e t where
  htraverse :: Applicative f => (e -> f b) -> (a -> f b) -> t a -> f (t b)

htraverse_hmap :: HTraverse e t => (e -> b) -> (a -> b) -> t a -> t b
htraverse_hmap eb ab ta = case htraverse (\e -> I (eb e)) (\a -> I (ab a)) ta of I tb -> tb

htraverse_hfoldMap :: (HTraverse e t,Add0 m) => (e -> m) -> (a -> m) -> t a -> m
htraverse_hfoldMap eb ab ta = case htraverse (\e -> K (eb e)) (\a -> K (ab a)) ta of K m -> m

class (HTraverse e t, HFold1 e t) => HTraverse1 e t where
  htraverse1 :: Apply f => (e -> f b) -> (a -> f b) -> t a -> f (t b)

htraverse_hfoldMap1 :: (HTraverse1 e t,Add m) => (e -> m) -> (a -> m) -> t a -> m
htraverse_hfoldMap1 eb ab ta = case htraverse1 (\e -> K (eb e)) (\a -> K (ab a)) ta of K m -> m

class (HTraverse e t, HFold0 e t) => HTraverse0 e t where
  htraverse0 :: Pure f => (e -> f b) -> (a -> f b) -> t a -> f (t b)

htraverse_hfoldMap0 :: (HTraverse0 e t,Zero m) => (e -> m) -> (a -> m) -> t a -> m
htraverse_hfoldMap0 eb ab ta = case htraverse0 (\e -> K (eb e)) (\a -> K (ab a)) ta of K m -> m

class (HTraverse0 e t, HTraverse1 e t, HFold_ e t) => HTraverse_ e t where
  htraverse_ :: (e -> f b) -> (a -> f b) -> t a -> f (t b)

htraverse_hfoldMap_ :: HTraverse_ e t => (e -> m) -> (a -> m) -> t a -> m
htraverse_hfoldMap_ eb ab ta = case htraverse_ (\e -> K (eb e)) (\a -> K (ab a)) ta of K m -> m

