module Traverse0 where
import Map
import AffFoldMap
import Pure
import Traverse
import K
import Def
import Sum

class (Traverse t,AffFoldMap t) => Traverse0 t where
  traverse0 :: Pure f => (a -> f b) -> t a -> f (t b)
  traverse0 f t = sequence0 (map f t)
  sequence0 :: Pure f => t (f a) -> f (t a)
  sequence0 = traverse0 (\x -> x)

foldMap0Default :: (Traverse0 t, Def m) => (a -> m) -> t a -> m
foldMap0Default f t = case traverse0 (\x -> K (f x)) t of {K m -> m}

instance Traverse0 ((,) x) where traverse0 f (x,a) = (x,) `map` f a
instance Traverse0 (E x) where
  traverse0 afb = \case
    L x -> pure (L x)
    R a -> map R (afb a)
instance Traverse (E x) where traverse = traverse0
instance AffFoldMap (E x)
instance FoldMap (E x)

instance Traverse0 (K x) where traverse0 f (K x) = pure (K x)
