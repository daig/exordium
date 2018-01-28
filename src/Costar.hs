module Costar (Costar(..),module X) where
import Comonad.Class as X
import Category.Class as X
import Closed.Class as X
import Instances

newtype Costar f a b = Costar {runCostar :: f a -> b}
syncTH

[instances| RMap (Costar f) where 
  rmap g (Costar fab) = Costar (\fa -> g (fab fa))|]
syncTH

[instances| Map f => Closed (Costar f) where
  closed (Costar fab) = Costar (\fxa x -> fab (map (\f -> f x) fxa))
  dimap f g (Costar fab) = Costar (dimap (map f) g fab)
  |]
instance Duplicate w => Compose (Costar w) where Costar f > Costar g = Costar (g < extend f)
instance Comonad w => Category (Costar w) where id = Costar fold_
