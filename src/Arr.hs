module Arr (Arr(..), module X) where
import Dimap as X
import Category as X

-- dimap f g (arr h) = arr (dimap f g h)
-- arr f < arr g = arr (f < g)
-- arr id = id
-- arr < const = const
class (Dimap p,Category p,Const p) => Arr p where
  arr :: (a -> b) -> p a b

arr_const :: Arr p => r -> p x r
arr_const r = arr (\_ -> r)

instance Arr (->) where arr = id
