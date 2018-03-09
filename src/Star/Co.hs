module Star.Co (Costar(..),module X) where
import Monad.Co as X
import Category as X
import Closed as X
import Map.Pro

newtype Costar f a b = Costar {runCostar :: f a -> b}

instance Map f => Closed (Costar f) where
  closed (Costar fab) = Costar (\fxa x -> fab (map (\f -> f x) fxa))
instance Map f => Promap (Costar f) where
  promap f g (Costar fab) = Costar (promap (map f) g fab)
instance Map f => ComapL (Costar f) where colmap = promap_colmap
instance MapR (Costar f) where rmap g (Costar fab) = Costar (\fa -> g (fab fa))
instance MapIso (Costar f a) where mapIso = map_mapIso
instance Map (Costar f a) where map = rmap_map
instance Duplicate w => Compose (Costar w) where Costar f > Costar g = Costar (g < extend f)
instance Comonad w => Category (Costar w) where id = Costar fold_

zipFOf :: (Costar f a b -> Costar f s t) -> (f a -> b) -> f s -> t
zipFOf g f = case g (Costar f) of Costar f' -> f'
