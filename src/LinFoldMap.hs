module LinFoldMap (module X, LinFoldMap(..)) where
import AffFoldMap as X
import RelFoldMap as X
import Sum as X
import Fun

class (AffFoldMap t, RelFoldMap t) =>  LinFoldMap t where
  {-# minimal foldMap_ | fold_ #-}
  foldMap_ :: (a -> b) -> t a -> b
  foldMap_ f x = f (fold_ x)
  fold_ :: t a -> a
  fold_ = foldMap_ (\x -> x)

  traverseL :: (a -> E a x) -> t a -> E (t a) x
  traverseL t = sequenceL < map t
  traverseR :: (a -> E x a) -> t a -> E x (t a)
  traverseR t = sequenceR < map t
  sequenceL :: t (E a b) -> E (t a) b 
  sequenceL t = case fold_ t of {L _ -> L $ map (\(L a) -> a) t; R b -> R b}
  sequenceR :: t (E a b) -> E a (t b)
  sequenceR t = case fold_ t of {L b -> L b; R _ -> R $ map (\(R b) -> b) t}

  traverseFst :: (a -> (a,x)) -> t a -> (t a, x)
  traverseFst f = sequenceFst < map f
  sequenceFst :: t (a,b) -> (t a, b)
  sequenceFst t = ((\(a,_) -> a) $@ t, fold_ $ (\(_,b) -> b) $@ t)
  traverseSnd :: (a -> (x,a)) -> t a -> (x, t a)
  traverseSnd t = sequenceSnd < map t
  sequenceSnd :: t (a,b) -> (a,t b)
  sequenceSnd t = (fold_ $ (\(a,_) -> a) $@ t, (\(_,b) -> b) $@ t)

instance LinFoldMap ((,) x) where foldMap_ f (x,a) = f a
