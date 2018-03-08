module Closed.Internal (Grating(..),module X) where
import Closed as X
import Map.Di
import Category

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

withGrate :: (Grating a b a b -> Grating a b s t) -> ((s -> a) -> b) -> t
withGrate g = case g (Grating (\f -> f (\x -> x))) of Grating z -> z

type (s &~  a) b t = forall p. Closed p => p a b -> p s t
type  s &~~ a      = forall p. Closed p => p a a -> p s s

type (s &~.  a) b t = Grating a b a b -> Grating a b s t
type  s &~~. a      = Grating a a a a -> Grating a a s s

--withGrate :: (s &~. a) b t -> ((s -> a) -> b) -> t

cloneGrate :: (s &~. a) b t -> (s &~ a) b t
cloneGrate g = grate (withGrate g)

