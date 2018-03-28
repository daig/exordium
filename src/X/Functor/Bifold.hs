module X.Functor.Bifold (module X.Functor.Bifold, module X) where
import X.Num.Add0 as X
import {-# source #-} X.ADT.E

class BifoldMap t where
  {-# minimal bifoldMap | bifoldr #-}
  bifoldMap :: Add0 m => (a -> m) -> (b -> m) -> t a b -> m
  bifoldMap f g t = bifoldr (\a m -> f a `add` m) (\b m -> g b `add` m) zero t -- TODO: check the order
  bifoldr :: (a -> x -> x) -> (b -> x -> x) -> x -> t a b -> x
  {-bifoldr c k z t = bifoldMap c k t z-} -- TODO: need an Add instance for (->)

class BifoldMap t => BifoldMap1 t where
  bifoldMap1 :: Add m => (a -> m) -> (b -> m) -> t a b -> m
  {-default bifoldMap1 :: (Bimap t, Add m) => (a -> m) -> (b -> m) -> t a b -> m-}
  {-bifoldMap1 f g t = biextract1 (bimap f g t)-}

-- superclass to BifoldMap1
{-class Biextract1 t where-} 
  {-biextract1 :: Add m => t m m -> m-}
  {-default biextract1 :: (BifoldMap1 t, Add m) => t m m -> m-}
  {-biextract1 = bifoldMap1 (\m -> m) (\m -> m)-}

class BifoldMap t => BifoldMap0 t where
  bifoldMap0 :: Zero m => (a -> m) -> (b -> m) -> t a b -> m

class (BifoldMap0 t, BifoldMap1 t) => BifoldMap_ t where
  bifoldMap_ :: (x -> a) -> (y -> a) -> t x y -> a


instance BifoldMap1 (,) where bifoldMap1 f g (a,b) = f a `add` g b
instance BifoldMap (,) where bifoldMap f g (a,b) = f a `add` g b

instance BifoldMap E where bifoldMap = bifoldMap_
instance BifoldMap0 E where bifoldMap0 = bifoldMap_
instance BifoldMap1 E where bifoldMap1 = bifoldMap_
instance BifoldMap_ E where
  bifoldMap_ f g = \case
    L a -> f a
    R b -> g b
