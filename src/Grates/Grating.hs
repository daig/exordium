module Grates.Grating (Grating(..), module X) where
import Closed.Class as X
import Dimap
import Category.Class
import Instances

newtype Grating a b s t = Grating (((s -> a) -> b) -> t)
syncTH


instance Closed (Grating a b) where
  closed (Grating z) = Grating (\f x -> z (\k -> f (\g -> k (g x))))

[instances| Dimap (Grating a b) where
  dimap f g (Grating z) = Grating (\d -> g (z (\k -> d (k < f))))|]
{-instance CoLMap (Grating a b) where colmap = dimap_colmap-}
{-instance RMap (Grating a b) where rmap = dimap_rmap-}
