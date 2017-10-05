module AffFoldable (AffFoldable(..),module X) where
import Foldable as X
import Def

class Foldable t => AffFoldable t where
  foldMap0 :: Def m => (a -> m) -> t a -> m
