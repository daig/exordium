module BiRelFoldMap (BiRelFoldMap(..), module X) where
import BiFoldMap as X
import Plus

class BiFoldMap t => BiRelFoldMap t where
  {-# minimal foldMapL1, foldMapR1  #-}
  foldMapL1 :: Plus s => (a -> s) -> t a b -> s
  foldMapR1 :: Plus s => (b -> s) -> t a b -> s
  bifoldMap1 :: Plus s => (a -> s) -> (b -> s) -> t a b -> s
  bifoldMap1 f g t = foldMapL1 f t + foldMapR1 g t
