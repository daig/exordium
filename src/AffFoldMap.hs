module AffFoldMap (AffFoldMap(..),module X) where
import FoldMap as X
import K

class FoldMap t => AffFoldMap t where
  foldMap0 :: Def m => (a -> m) -> t a -> m

instance AffFoldMap ((,) x) where foldMap0 f (x,a) = f a

instance AffFoldMap (K x) where foldMap0 _ _ = def
