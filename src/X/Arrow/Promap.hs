{-# language MagicHash #-}
module X.Arrow.Promap (module X.Arrow.Promap, module X) where
import X.Type.NatTrans
import X.Cast.Coerce
import X.Functor.Map as X
import X.Functor.Comap as X
import X.Type.Permute as X (BA(..))

id :: x -> x
id x = x

class (forall a. Map (p a), forall b. Comap (BA p b)) => Promap p where
  {-# minimal promap | premap,postmap #-}
  promap :: (s -> a) -> (b -> t) -> p a b -> p s t
  promap f g = \p -> postmap g (premap f p)
  -- | Strictly @promap@ with functions that are assumed
  -- operationally to be a cast, such as a newtype
  -- constructor.
  --
  -- /Note:/ This operation is explicitly /unsafe/
  -- since an implementation may choose to use
  -- 'unsafeCoerce' to implement this combinator
  -- and it has no way to validate that your function
  -- meets the requirements.
  --
  -- If you implement this combinator with
  -- 'unsafeCoerce', then you are taking upon yourself
  -- the obligation that you don't use GADT-like
  -- tricks to distinguish values.
  promap# :: forall s a b t. (s #=# a, b #=# t) => (s -> a) -> (b -> t) -> p a b -> p s t
  {-# INLINE promap# #-}
  promap# _ _ !p = promap coerce coerce p -- TODO: fiddle with arguments to get good inlining
  premap :: (a -> x) -> p x b -> p a b
  premap = (`promap` \b -> b)
  premap# :: forall s a t. s #=# a => (s -> a) -> p a t -> p s t
  {-# INLINE premap# #-}
  premap# f = promap# f id
  postmap :: (y -> b) -> p a y -> p a b
  postmap = map
  postmap# :: forall y b a. y #=# b => (y -> b) -> p a y -> p a b
  {-# INLINE postmap# #-}
  postmap# = promap# id


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

promap_comap :: Promap p => (a' -> a) -> BA p b a -> BA p b a'
promap_comap a'a = _BA (promap a'a id) where
  _BA :: Promap p => p (f b a) (f y x) -> p (BA f a b) (BA f x y)
  _BA = promap# coerce coerce
