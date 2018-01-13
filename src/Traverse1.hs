module Traverse1 (Traverse1(..), foldMap1Default, module X) where
import Map as X
import FoldMap1 as X
import Traverse as X
import K

class (Traverse t,FoldMap1 t) => Traverse1 t where
  {-# minimal traverse1 | sequence1 #-}
  traverse1 :: Apply f => (a -> f b) -> t a -> f (t b)
  traverse1 f t = sequence1 (map f t)
  sequence1 :: Apply f => t (f a) -> f (t a)
  sequence1 = traverse1 (\x -> x)

foldMap1Default :: (Traverse1 t, Plus m) => (a -> m) -> t a -> m
foldMap1Default f t = case traverse1 (\x -> K (f x)) t of {K m -> m}

instance Traverse1 ((,) x) where traverse1 f (x,a) = (x,) `map` f a
