module Foldable where
import Plus

class Foldable t where
  foldMap :: Zero m => (a -> m) -> t a -> m
  {-foldr :: (a -> b -> b) -> b -> t a -> b-}
  {-foldl :: (b -> a -> b) -> b -> t a -> b-}

class Foldable t => Foldable0 t where
  fold0 :: (a -> b) -> b -> t a -> b
class Foldable t => Foldable1 t where
  foldMap1 :: Plus s => (a -> s) -> t s -> s
