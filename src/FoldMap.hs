module FoldMap (module FoldMap, module X) where
import PlusZero as X
import List
import {-# source #-} K
import {-# source #-} I

class FoldMap t where
  {-# minimal foldMap | foldr #-}
  foldMap :: PlusZero m => (a -> m) -> t a -> m
  foldMap f t = foldr (\a m -> f a `plus` m) zero t -- TODO: check the order
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldr c z t = foldMap c t z
  {-foldl :: (b -> a -> b) -> b -> t a -> b-}

class FoldMap t => FoldMap0 t where
  foldMap0 :: Zero m => (a -> m) -> t a -> m

class FoldMap t => FoldMap1 t where
  foldMap1 :: Plus s => (a -> s) -> t a -> s

class (FoldMap0 t, FoldMap1 t) =>  FoldMap_ t where
  {-# minimal foldMap_ | fold_ #-}
  foldMap_ :: (a -> b) -> t a -> b
  foldMap_ f x = f (fold_ x)
  fold_ :: t a -> a
  fold_ = foldMap_ (\x -> x)

  {-traverseL :: (a -> E a x) -> t a -> E (t a) x-}
  {-traverseL t = sequenceL < map t-}
  {-traverseR :: (a -> E x a) -> t a -> E x (t a)-}
  {-traverseR t = sequenceR < map t-}
  {-sequenceL :: t (E a b) -> E (t a) b -}
  {-sequenceL t = case fold_ t of {L _ -> L $ map (\(L a) -> a) t; R b -> R b}-}
  {-sequenceR :: t (E a b) -> E a (t b)-}
  {-sequenceR t = case fold_ t of {L b -> L b; R _ -> R $ map (\(R b) -> b) t}-}

  {-traverseFst :: (a -> (a,x)) -> t a -> (t a, x)-}
  {-traverseFst f = sequenceFst < map f-}
  {-sequenceFst :: t (a,b) -> (t a, b)-}
  {-sequenceFst t = ((\(a,_) -> a) $@ t, fold_ $ (\(_,b) -> b) $@ t)-}
  {-traverseSnd :: (a -> (x,a)) -> t a -> (x, t a)-}
  {-traverseSnd t = sequenceSnd < map t-}
  {-sequenceSnd :: t (a,b) -> (a,t b)-}
  {-sequenceSnd t = (fold_ $ (\(a,_) -> a) $@ t, (\(_,b) -> b) $@ t)-}


instance FoldMap0 (K x) where foldMap0 = \_ _ -> zero
instance FoldMap (K x) where foldMap = foldMap0

instance FoldMap_ ((,) x) where foldMap_ f (_,y) = f y
instance FoldMap0 ((,) x) where foldMap0 = foldMap_
instance FoldMap1 ((,) x) where foldMap1 = foldMap_
instance FoldMap ((,) x) where foldMap = foldMap_

instance FoldMap [] where foldMap = list'foldMap zero plus

instance FoldMap_ I where foldMap_ f (I a) = f a
instance FoldMap0 I where foldMap0 = foldMap_
instance FoldMap1 I where foldMap1 = foldMap_
instance FoldMap  I where foldMap = foldMap_
