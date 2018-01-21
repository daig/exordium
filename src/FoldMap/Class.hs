module FoldMap.Class (FoldMap(..), module X) where
import PlusZero.Class as X
import Utils.K
import Utils.List
import Utils.Tuple
import Utils.I

class FoldMap t where
  {-# minimal foldMap | foldr #-}
  foldMap :: PlusZero m => (a -> m) -> t a -> m
  foldMap f t = foldr (\a m -> f a `plus` m) zero t -- TODO: check the order
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldr c z t = foldMap c t z
  {-foldl :: (b -> a -> b) -> b -> t a -> b-}

instance FoldMap ((,) x) where foldMap f (_,a) = f a
instance FoldMap [] where foldMap = list'foldMap zero plus
instance FoldMap (K x) where foldMap = \_ _ -> zero
instance FoldMap I where foldMap = i'foldMap_
