module AGrate
  (type (&~.), type (&~~.)
  ,withGrate, cloneGrate, zipFWithOf, collectOf
  ,module X) where
import Grate as X
import Grate.Grating as X
import Costar as X
import Star as X
import Distributive as X
import Zipping as X

type (s &~.  a) b t = Grating a b a b -> Grating a b s t
type  s &~~. a      = Grating a a a a -> Grating a a s s

withGrate :: (s &~. a) b t -> ((s -> a) -> b) -> t
withGrate g = case g (Grating (\f -> f (\x -> x))) of Grating z -> z

cloneGrate :: (s &~. a) b t -> (s &~ a) b t
cloneGrate g = grate (withGrate g)

zipFWithOf :: (Costar f a b -> Costar f s t) -> (f a -> b) -> f s -> t
zipFWithOf g f = case g (Costar f) of Costar f' -> f'

zipWithOf :: (Zipping a b -> Zipping s t) -> (a -> a -> b) -> s -> s -> t
zipWithOf l z = case l (Zipping z) of Zipping z' -> z'

collectOf :: (Star f a b -> Star f s t) -> (a -> f b) -> s -> f t
collectOf g f = case g (Star f) of Star f' -> f'