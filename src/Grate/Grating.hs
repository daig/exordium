module Grate.Grating (Grating(..), module X) where
import Closed as X
import Category

newtype Grating a b s t = Grating (((s -> a) -> b) -> t)

instance Closed (Grating a b) where
  closed (Grating z) = Grating (\f x -> z (\k -> f (\g -> k (g x))))

instance Dimap (Grating a b) where
  dimap f g (Grating z) = Grating (\d -> g (z (\k -> d (k < f))))
