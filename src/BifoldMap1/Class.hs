module BifoldMap1.Class (BifoldMap1(..), module X) where
import PlusZero.Class as X
import BifoldMap.Class as X

class BifoldMap t => BifoldMap1 t where
  bifoldMap1 :: Plus m => (a -> m) -> (b -> m) -> t a b -> m
  {-default bifoldMap1 :: (Bimap t, Plus m) => (a -> m) -> (b -> m) -> t a b -> m-}
  {-bifoldMap1 f g t = biextract1 (bimap f g t)-}

-- superclass to BifoldMap1
{-class Biextract1 t where-} 
  {-biextract1 :: Plus m => t m m -> m-}
  {-default biextract1 :: (BifoldMap1 t, Plus m) => t m m -> m-}
  {-biextract1 = bifoldMap1 (\m -> m) (\m -> m)-}

instance BifoldMap1 (,) where bifoldMap1 f g (a,b) = f a `plus` g b
