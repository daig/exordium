module Iso
  (type (=~), type (=~~), type (=:~)
  ,iso, iso', isoF
  ,module X) where
import Equality as X
import Dimap as X
import NatTrans as X (type (~>))

type (s =~  a) b t = forall p. Dimap p => p a b -> p s t
type  s =~~ a      = forall p. Dimap p => p a a -> p s s

iso :: (s -> a) -> (b -> t) -> (s =~ a) b t
iso = dimap
{-# inline iso #-}
iso' :: (a -> b) -> (b -> a) -> a =~~ b
iso' = dimap
{-# inline iso' #-}

type f =:~ g = forall p x y. Dimap p => p (g x) (g y) -> p (f x) (f y)
isoF :: f~>g -> g~>f -> f=:~g
isoF = dimap
{-# inline isoF #-}
