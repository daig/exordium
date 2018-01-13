module FoldMap0 (FoldMap0(..),module X) where
import FoldMap as X
import K

class FoldMap t => FoldMap0 t where
  foldMap0 :: Def m => (a -> m) -> t a -> m

instance FoldMap0 ((,) x) where foldMap0 f (x,a) = f a

instance FoldMap0 (K x) where foldMap0 _ _ = def
