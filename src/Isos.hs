module Isos
  (module Isos
  ,module X) where
import NatTrans as X
import Dimap as X
import Isos.AnIso as X (AnIso)
import Isos.AnIso
import Isos.Re

re :: (Re p s t s t -> Re p s t a b) -> p b a -> p t s
re l = runRe (l (Re (\p -> p)))

type (s ~=. a) b t = AnIso a b a b -> AnIso a b s t
withIso :: (s ~=. a) b t -> ((s -> a) -> (b -> t) -> r) -> r
withIso ai k = case ai (AnIso (\x -> x) (\x -> x)) of {AnIso sa bt -> k sa bt}
under :: (s ~=. a) b t -> (t -> s) -> b -> a
under k = withIso k (\sa bt ts x -> sa (ts (bt x)))

type (s ~= a) b t = forall p. Dimap p => p a b -> p s t
iso :: (s -> a) -> (b -> t) -> (s ~= a) b t
iso = dimap
{-# inline iso #-}

type a ~== b = forall p. Dimap p => p a a -> p b b
iso' :: (b -> a) -> (a -> b) -> a~==b
iso' = dimap
{-# inline iso' #-}

type f ~~= g = forall p x y. Dimap p => p (g x) (g y) -> p (f x) (f y)
isoF :: f-->g -> g-->f -> f~~=g
isoF = dimap
{-# inline isoF #-}

type p ~~~= q = forall i a b x y. Dimap i => i (q x y) (q a b) -> i (p x y) (p a b)
isoP :: p--->q -> q--->p -> p~~~=q
isoP = dimap
