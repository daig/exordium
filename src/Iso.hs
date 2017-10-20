module Iso
  (type (=~), type (=~~)
  ,iso
  ,module X) where
import Equality as X
import Dimap as X
import Map as X

type (s =~ a) b t = forall p f. (Dimap p, Map f) => p a (f b) -> p s (f t)
type (s =~~ a) = forall p f. (Dimap p, Map f) => p a (f a) -> p s (f s)

iso :: (s -> a) -> (b -> t) -> (s =~ a) b t
iso sa bt = dimap sa (map bt)
{-# inline iso #-}



