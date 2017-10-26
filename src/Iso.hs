module Iso
  (type (=~), type (=~~), type (=:~), type (=::~)
  ,iso, iso', isoF, isoP
  ,re
  ,module X) where
import Equality as X
import Dimap as X
import NatTrans as X
import AnIso.Re

type (s =~  a) b t = forall p. Dimap p => p a b -> p s t
iso :: (s -> a) -> (b -> t) -> (s =~ a) b t
iso = dimap
{-# inline iso #-}

type  s =~~ a      = forall p. Dimap p => p a a -> p s s
iso' :: (a -> b) -> (b -> a) -> a =~~ b
iso' = dimap
{-# inline iso' #-}

type f =:~ g = forall p x y. Dimap p => p (g x) (g y) -> p (f x) (f y)
isoF :: f-->g -> g-->f -> f=:~g
isoF = dimap
{-# inline isoF #-}

type f =::~ g = forall p a b x y. Dimap p => p (g x y) (g a b) -> p (f x y) (f a b)
isoP :: f--->g -> g--->f -> f=::~g
isoP = dimap

re :: (Re p s t s t -> Re p s t a b) -> p b a -> p t s
re l = runRe (l (Re (\x -> x)))
