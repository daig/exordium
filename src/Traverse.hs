module Traverse (module Traverse, module X) where
import FoldMap as X
import Applicative.Class as X
import {-# source #-} K
import I

class (Map t,FoldMap t) => Traverse t where
  {-# minimal traverse | cocollect | sequence #-}
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse f t = cocollect (\x -> x) (map f t)
  cocollect :: Applicative f => (t a -> b) -> t (f a) -> f b
  cocollect tab tfa = map tab (sequence tfa)
  sequence :: Applicative f => t (f a) -> f (t a)
  sequence = traverse (\x -> x)


traverse_map :: Traverse t => (a -> b) -> t a -> t b
traverse_map f ta = case traverse (\a -> I (f a)) ta of I tb -> tb

traverse_foldMap :: (Traverse t,PlusZero m) => (a -> m) -> t a -> m
traverse_foldMap f ta = case traverse (\a -> K (f a)) ta of K m -> m

class (Traverse t,FoldMap0 t) => Traverse0 t where
  traverse0 :: Pure f => (a -> f b) -> t a -> f (t b)
  traverse0 f t = sequence0 (map f t)
  sequence0 :: Pure f => t (f a) -> f (t a)
  sequence0 = traverse0 (\x -> x)


traverse0_foldMap0 :: (Traverse0 t,Zero m) => (a -> m) -> t a -> m
traverse0_foldMap0 f ta = case traverse0 (\a -> K (f a)) ta of K m -> m

class (Traverse t,FoldMap1 t) => Traverse1 t where
  {-# minimal traverse1 | sequence1 #-}
  traverse1 :: Apply f => (a -> f b) -> t a -> f (t b)
  traverse1 f t = sequence1 (map f t)
  sequence1 :: Apply f => t (f a) -> f (t a)
  sequence1 = traverse1 (\x -> x)


traverse1_foldMap1 :: (Traverse1 t,Plus m) => (a -> m) -> t a -> m
traverse1_foldMap1 f ta = case traverse1 (\a -> K (f a)) ta of K m -> m

class (Traverse0 t, Traverse1 t,FoldMap_ t) => Traverse_ t where
  traverse_ :: Map f => (a -> f b) -> t a -> f (t b)
  traverse_ f t = sequence_ (map f t)
  sequence_ :: Map f => t (f a) -> f (t a)
  sequence_ = traverse_ (\x -> x)


traverse__foldMap_ :: Traverse_ t => (a -> m) -> t a -> m
traverse__foldMap_ f ta = case traverse_ (\a -> K (f a)) ta of K m -> m

instance Traverse0 ((,) x) where traverse0 f (x,a) = (x,) `map` f a
instance Traverse1 ((,) x) where traverse1 f (x,a) = (x,) `map` f a
instance Traverse_ ((,) x) where traverse_ f (x,a) = (x,) `map` f a
instance Traverse ((,) x) where traverse f (x,a) = (x,) `map` f a
instance Traverse [] where
  traverse = go' where
    go' f = go where
      go = \case
	[] -> pure []
	(x:xs) -> (:) `map` f x  `ap` go xs
instance Traverse (K x) where traverse f (K x) = pure (K x)
instance Traverse0 (K x) where traverse0 f (K x) = pure (K x)
