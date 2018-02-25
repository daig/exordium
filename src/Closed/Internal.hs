module Closed.Internal (Grating(..),module X) where
import Closed.Class as X
import Dimap
import Category.Class

newtype Grating a b s t = Grating {runGrating :: (((s -> a) -> b) -> t)}
{-_Grating = dimap runGrating Grating-}
{-collectOf :: (Star f a b -> Star f s t) -> (a -> f b) -> s -> f t-}
{-collectOf g f = case g (Star f) of Star f' -> f'-}

instance Closed (Grating a b) where
  closed (Grating z) = Grating (\f x -> z (\k -> f (\g -> k (g x))))

instance Dimap (Grating a b) where
  dimap f g (Grating z) = Grating (\d -> g (z (\k -> d (k < f))))
instance ComapL (Grating a b) where colmap = dimap_colmap
instance MapR (Grating a b) where rmap = dimap_rmap
