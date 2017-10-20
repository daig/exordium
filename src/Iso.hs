module Iso
  (type (=~), type (=~~), type (=:~)
  ,iso, isoF
  ,module X) where
import Equality as X
import Dimap as X
import Map as X
import NatTrans as X (type (~>))

type (s =~ a) b t = forall p f. (Dimap p, Map f) => p a (f b) -> p s (f t)
type (s =~~ a) = forall p f. (Dimap p, Map f) => p a (f a) -> p s (f s)

iso :: (s -> a) -> (b -> t) -> (s =~ a) b t
iso sa bt = dimap sa (map bt)
{-# inline iso #-}


type f =:~ g = forall p h x y. (Dimap p, Map h) => p (g x) (h (g y)) -> p (f x) (h (f y))
isoF :: f~>g -> g~>f -> f=:~g
isoF sa bt = dimap sa (map bt)
{-# inline isoF #-}
