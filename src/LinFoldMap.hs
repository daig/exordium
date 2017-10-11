module LinFoldMap (module X, LinFoldMap(..)) where
import AffFoldMap as X
import RelFoldMap as X

class (AffFoldMap t, RelFoldMap t) =>  LinFoldMap t where
  {-# minimal foldMap_ | fold_ #-}
  foldMap_ :: (a -> b) -> t a -> b
  foldMap_ f x = f (fold_ x)
  fold_ :: t a -> a
  fold_ = foldMap_ (\x -> x)
