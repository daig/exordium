module FoldMap (FoldMap(..), module X) where
import Zero as X (Zero,Def,Plus)
import Zero

class FoldMap t where
  {-# minimal foldMap | foldr #-}
  foldMap :: Zero m => (a -> m) -> t a -> m
  foldMap f t = foldr (\a m -> f a + m) zero t -- TODO: check the order
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldr c z t = foldMap c t z
  {-foldl :: (b -> a -> b) -> b -> t a -> b-}

instance FoldMap ((,) x) where foldMap f (_,a) = f a
instance FoldMap [] where
  foldMap f = go where
    go = \case
      [] -> def
      a:as -> f a + go as
