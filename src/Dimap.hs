module Dimap (module Dimap, module X) where
import Dimap.Class as X
import RMap as X
import NatTrans as X
{-import Dimap.Class as X-}
{-import AnIso as X (AnIso)-}
{-import AFold as X -- TODO: move to Folds.hs-}
{-import Prisms as X-}
import Dimap.Internal
import Dimap.Internal as X (AnIso)
import Re.Type as X (Re)
import Re.Type
import Instances


defaulting 'rmap [|dimap (\a -> a)|]
defaulting 'colmap [|\f -> dimap f (\b -> b)|]
{-dimap_comap :: forall p a x b. Dimap p => (a -> x) -> Flipped p b x -> Flipped p b a-}
{-dimap_comap f p = coerce# (dimap f (\b -> b) (coerce# @(p x b) p))-}

{-(!<) :: Dimap p => b -> p a y -> p a b-}
{-(!<) = constpostmap-}
{-(>!) :: Dimap p => p a y -> b -> p a b-}
{-p >! b = constpostmap b p-}
{-(!>@) :: Dimap p => x -> p x b -> p a b-}
{-(!>@) = constpremap-}
{-(@<!) :: Dimap p => p x b -> x -> p a b-}
{-p @<! x = constpremap x p-}

{-(<#) :: Dimap p => (y -> b) -> p a y -> p a b-}
{-(>#) :: Dimap p => (a -> x) -> p x b -> p a b-}
{-(#<) :: Dimap p => p x b -> (a -> x) -> p a b-}
{-(#>) :: Dimap p => p a y -> (y -> b) -> p a b-}
{-(<#) = postmap-}
{-(>#) = premap-}
{-p #< f = premap f p-}
{-p #> f = postmap f p-}


{-(<@) :: Dimap p => (y -> b) -> p a y -> p a b-}
{-(>@) :: Dimap p => (a -> x) -> p x b -> p a b-}
{-(@<) :: Dimap p => p x b -> (a -> x) -> p a b-}
{-(@>) :: Dimap p => p a y -> (y -> b) -> p a b-}
{-(<@) = postmap-}
{-(>@) = premap-}
{-p @< f = premap f p-}
{-p @> f = postmap f p-}

(>@>) :: Dimap p => (a -> x) -> (y -> b) -> p x y -> p a b
(>@>) = dimap

(>|) :: Dimap p => (a -> x) -> p x y -> (y -> b) -> p a b
(|>) :: ((y -> b) -> p a b) -> (y -> b) -> p a b
(f >| p) g = dimap f g p
k |> f = k f

(<|) :: Dimap p => (y -> b) -> p x y -> (a -> x) -> p a b
(|<) :: ((a -> x) -> p a b) -> (a -> x) -> p a b
(g <| p) f = dimap f g p
k |< f = k f

(^!) :: ((x -> a) -> r) -> a -> r
l ^! a = l (\_ -> a)

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
{-# inline isoP #-}


re :: (Re p s t s t -> Re p s t a b) -> p b a -> p t s
re l = runRe (l (Re (\p -> p)))

type (s ~=. a) b t = AnIso a b a b -> AnIso a b s t
withIso :: (s ~=. a) b t -> ((s -> a) -> (b -> t) -> r) -> r
withIso ai k = case ai (AnIso (\x -> x) (\x -> x)) of {AnIso sa bt -> k sa bt}
under :: (s ~=. a) b t -> (t -> s) -> b -> a
under k = withIso k (\sa bt ts x -> sa (ts (bt x)))
