module Arr (Arr(..), module X) where
import Dimap as X
import Category as X

-- dimap f g (arr h) = arr (dimap f g h)
-- arr id = id
class (Dimap p,Category p) => Arr p where
  {-# minimal arr #-}
  arr :: (a -> b) -> p a b
  constP :: b -> p x b
  constP = \b -> arr (\_ -> b)

instance Arr (->) where
  arr = id
  constP = \b _ -> b
