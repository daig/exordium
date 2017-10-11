module Bifoldable (Bifoldable(..), module X) where
import Zero as X

class (FoldableL t, FoldableR t) => Bifoldable t where
  {-# minimal bifoldmap | foldmapl, foldmapr #-}
  bifoldMap :: Zero m => (a -> m) -> (b -> m) -> t a b -> m
  foldMapL :: Zero m => (a -> m) -> t a b -> m
  foldMapR :: Zero m => (b -> m) -> t a b -> m
  {-foldr :: (a -> b -> b) -> b -> t a -> b-}
  {-foldl :: (b -> a -> b) -> b -> t a -> b-}
