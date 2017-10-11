module AffFoldMap (AffFoldMap(..),module X) where
import FoldMap as X

class FoldMap t => AffFoldMap t where
  foldMap0 :: Def m => (a -> m) -> t a -> m
