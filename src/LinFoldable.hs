module LinFoldable (module X, LinFoldable(..)) where
import AffFoldable as X
import RelFoldable as X

class (AffFoldable t, RelFoldable t) =>  LinFoldable t where
  {-# minimal foldMap_ | fold_ #-}
  foldMap_ :: (a -> b) -> t a -> b
  foldMap_ f x = f (fold_ x)
  fold_ :: t a -> a
  fold_ = foldMap_ (\x -> x)
