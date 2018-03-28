module X.Functor.Traverse (module X.Functor.Traverse, module X) where
import X.Functor.Fold as X
import X.Functor.Applicative as X
import X.Functor.Comonad as X
import {-# source #-} X.Type.K
import X.Type.I
import X.ADT.Maybe

class (Map t,Fold t) => Traverse t where
  {-# minimal traverse | cocollect | sequence #-}
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse f t = cocollect (\x -> x) (map f t)
  cocollect :: Applicative f => (t a -> b) -> t (f a) -> f b
  cocollect tab tfa = map tab (sequence tfa)
  sequence :: Applicative f => t (f a) -> f (t a)
  sequence = traverse (\x -> x)


traverse_map :: Traverse t => (a -> b) -> t a -> t b
traverse_map f ta = case traverse (\a -> I (f a)) ta of I tb -> tb

traverse_foldMap :: (Traverse t,Add0 m) => (a -> m) -> t a -> m
traverse_foldMap f ta = case traverse (\a -> K (f a)) ta of K m -> m

class (Traverse t,Fold0 t) => Traverse0 t where
  traverse0 :: Pure f => (a -> f b) -> t a -> f (t b)
  traverse0 f t = sequence0 (map f t)
  sequence0 :: Pure f => t (f a) -> f (t a)
  sequence0 = traverse0 (\x -> x)

class (Traverse0 t, Fold' t) => Traverse' t where
  traverse' :: Map f => (t X -> f X) -> (a -> f b) -> t a -> f (t b)
  sequence' :: Map f => (t X -> f X) -> t (f a) -> f (t a)
  {-traverse' x a ta = map foldMap' x a ta-}
instance Traverse' (E x) where
  traverse' txfx afb = \case {L x -> map (\case) (txfx (L x)); R a -> R `map` afb a}
  sequence' txfx = \case {L x -> map (\case) (txfx (L x)); R fa -> R `map` fa}


traverse0_foldMap0 :: (Traverse0 t,Zero m) => (a -> m) -> t a -> m
traverse0_foldMap0 f ta = case traverse0 (\a -> K (f a)) ta of K m -> m

class (Traverse t,Fold1 t) => Traverse1 t where
  {-# minimal traverse1 | sequence1 #-}
  traverse1 :: Apply f => (a -> f b) -> t a -> f (t b)
  traverse1 f t = sequence1 (map f t)
  sequence1 :: Apply f => t (f a) -> f (t a)
  sequence1 = traverse1 (\x -> x)


traverse1_foldMap1 :: (Traverse1 t,Add m) => (a -> m) -> t a -> m
traverse1_foldMap1 f ta = case traverse1 (\a -> K (f a)) ta of K m -> m

class (Traverse0 t, Traverse1 t,Fold_ t, Comonad t) => Traverse_ t where
  traverse_ :: Map f => (a -> f b) -> t a -> f (t b)
  traverse_ f t = sequence_ (map f t)
  sequence_ :: Map f => t (f a) -> f (t a)
  sequence_ = traverse_ (\x -> x)

traverse__duplicate :: Traverse_ w => w a -> w (w a)
traverse__duplicate w = w `constMap` w
traverse__extend wab wa = wab wa `constMap` wa


traverse__foldMap_ :: Traverse_ t => (a -> m) -> t a -> m
traverse__foldMap_ f ta = case traverse_ (\a -> K (f a)) ta of K m -> m

instance Duplicate I where duplicate = I
instance Comonad I
instance Traverse I where traverse = traverse_
instance Traverse0 I where traverse0 = traverse_
instance Traverse1 I where traverse1 = traverse_
instance Traverse_ I where traverse_ afb (I a) = I `map` afb a

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

instance Traverse (E x) where traverse = traverse0
instance Traverse0 (E x) where
  traverse0 f = \case
    L x -> pure (L x)
    R a -> R `map` f a


instance Traverse0 Maybe where
  traverse0 afb = \case
    Nothing -> pure Nothing
    Just a -> Just `map` afb a
instance Traverse Maybe where traverse = traverse0
instance Traverse' Maybe where
  traverse' f0 f = \case {Nothing -> map (\case) (f0 Nothing); Just a -> Just `map` f a}

instance Traverse' I where
  traverse' _ f (I a) = I `map` f a
