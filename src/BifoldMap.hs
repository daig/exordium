module BifoldMap (BifoldMap(..), module X) where
import Zero as X

class BifoldMap t where
  {-# minimal bifoldMap | foldMapL, foldMapR #-}
  bifoldMap :: Zero m => (a -> m) -> (b -> m) -> t a b -> m
  foldMapL :: Zero m => (a -> m) -> t a b -> m
  foldMapR :: Zero m => (b -> m) -> t a b -> m
  {-foldr :: (a -> b -> b) -> b -> t a -> b-}
  {-foldl :: (b -> a -> b) -> b -> t a -> b-}
