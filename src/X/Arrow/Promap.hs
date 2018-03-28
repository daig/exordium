module X.Arrow.Promap (module X.Arrow.Promap) where
import X.Type.NatTrans

class Promap p where
  promap :: (s -> a) -> (b -> t) -> p a b -> p s t
  promap f g = \p -> postmap g (premap f p)
  premap :: (a -> x) -> p x b -> p a b
  premap = (`promap` \b -> b)
  postmap :: (y -> b) -> p a y -> p a b
  postmap = promap (\a -> a)

instance Promap (->) where promap f g p = \a -> g (p (f a))


promap_rmap :: Promap p => (x -> b) -> p a x -> p a b
promap_rmap = promap (\a -> a)
promap_colmap :: Promap p => (a -> x) -> p x b -> p a b
promap_colmap = (`promap` (\b -> b))
{-promap_comap :: forall p a x b. Promap p => (a -> x) -> Flipped p b x -> Flipped p b a-}
{-promap_comap f p = coerce# (promap f (\b -> b) (coerce# @(p x b) p))-}

{-(!<) :: Promap p => b -> p a y -> p a b-}
{-(!<) = constpostmap-}
{-(>!) :: Promap p => p a y -> b -> p a b-}
{-p >! b = constpostmap b p-}
{-(!>@) :: Promap p => x -> p x b -> p a b-}
{-(!>@) = constpremap-}
{-(@<!) :: Promap p => p x b -> x -> p a b-}
{-p @<! x = constpremap x p-}

{-(<#) :: Promap p => (y -> b) -> p a y -> p a b-}
{-(>#) :: Promap p => (a -> x) -> p x b -> p a b-}
{-(#<) :: Promap p => p x b -> (a -> x) -> p a b-}
{-(#>) :: Promap p => p a y -> (y -> b) -> p a b-}
{-(<#) = postmap-}
{-(>#) = premap-}
{-p #< f = premap f p-}
{-p #> f = postmap f p-}


{-(<@) :: Promap p => (y -> b) -> p a y -> p a b-}
{-(>@) :: Promap p => (a -> x) -> p x b -> p a b-}
{-(@<) :: Promap p => p x b -> (a -> x) -> p a b-}
{-(@>) :: Promap p => p a y -> (y -> b) -> p a b-}
{-(<@) = postmap-}
{-(>@) = premap-}
{-p @< f = premap f p-}
{-p @> f = postmap f p-}

(>@>) :: Promap p => (a -> x) -> (y -> b) -> p x y -> p a b
(>@>) = promap

(>|) :: Promap p => (a -> x) -> p x y -> (y -> b) -> p a b
(|>) :: ((y -> b) -> p a b) -> (y -> b) -> p a b
(f >| p) g = promap f g p
k |> f = k f

(<|) :: Promap p => (y -> b) -> p x y -> (a -> x) -> p a b
(|<) :: ((a -> x) -> p a b) -> (a -> x) -> p a b
(g <| p) f = promap f g p
k |< f = k f

(^!) :: ((x -> a) -> r) -> a -> r
l ^! a = l (\_ -> a)

{-type (s ~= a) b t = forall p. Promap p => p a b -> p s t-}
{-iso :: (s -> a) -> (b -> t) -> (s ~= a) b t-}
{-iso = promap-}
{-{-# inline iso #-}-}

{-type a ~== b = forall p. Promap p => p a a -> p b b-}
{-iso' :: (b -> a) -> (a -> b) -> a~==b-}
{-iso' = promap-}
{-{-# inline iso' #-}-}

type f ~~= g = forall p x y. Promap p => p (g x) (g y) -> p (f x) (f y)
isoF :: f-->g -> g-->f -> f~~=g
isoF = promap
{-# inline isoF #-}

{-type p ~~~= q = forall i a b x y. Promap i => i (q x y) (q a b) -> i (p x y) (p a b)-}
{-isoP :: p--->q -> q--->p -> p~~~=q-}
{-isoP = promap-}
{-{-# inline isoP #-}-}



--type (s ~=. a) b t = AnIso a b a b -> AnIso a b s t
--withIso :: (s ~=. a) b t -> ((s -> a) -> (b -> t) -> r) -> r
--withIso ai k = case ai (AnIso (\x -> x) (\x -> x)) of {AnIso sa bt -> k sa bt}
--under :: (s ~=. a) b t -> (t -> s) -> b -> a
--under k = withIso k (\sa bt ts x -> sa (ts (bt x)))
