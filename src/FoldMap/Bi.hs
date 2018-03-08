module FoldMap.Bi (module FoldMap.Bi, module X) where
import PlusZero as X

class BifoldMap t where
  {-# minimal bifoldMap | bifoldr #-}
  bifoldMap :: PlusZero m => (a -> m) -> (b -> m) -> t a b -> m
  bifoldMap f g t = bifoldr (\a m -> f a `plus` m) (\b m -> g b `plus` m) zero t -- TODO: check the order
  bifoldr :: (a -> x -> x) -> (b -> x -> x) -> x -> t a b -> x
  bifoldr c k z t = bifoldMap c k t z

class BifoldMap t => BifoldMap1 t where
  bifoldMap1 :: Plus m => (a -> m) -> (b -> m) -> t a b -> m
  {-default bifoldMap1 :: (Bimap t, Plus m) => (a -> m) -> (b -> m) -> t a b -> m-}
  {-bifoldMap1 f g t = biextract1 (bimap f g t)-}

-- superclass to BifoldMap1
{-class Biextract1 t where-} 
  {-biextract1 :: Plus m => t m m -> m-}
  {-default biextract1 :: (BifoldMap1 t, Plus m) => t m m -> m-}
  {-biextract1 = bifoldMap1 (\m -> m) (\m -> m)-}

class BifoldMap t => BifoldMap0 t where
  bifoldMap0 :: Zero m => (a -> m) -> (b -> m) -> t a b -> m

class (BifoldMap0 t, BifoldMap1 t) => BifoldMap_ t where
  bifoldMap_ :: (x -> a) -> (y -> a) -> t x y -> a


instance BifoldMap1 (,) where bifoldMap1 f g (a,b) = f a `plus` g b
instance BifoldMap (,) where bifoldMap f g (a,b) = f a `plus` g b
