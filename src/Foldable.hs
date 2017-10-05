module Foldable (Foldable(..), module X) where
import Zero as X

class Foldable t where
  foldMap :: Zero m => (a -> m) -> t a -> m
  {-foldr :: (a -> b -> b) -> b -> t a -> b-}
  {-foldl :: (b -> a -> b) -> b -> t a -> b-}
