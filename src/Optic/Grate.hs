module Optic.Grate (module Optic.Grate, module X) where
import Closed as X
import Category as X
import Monad.Co as X

newtype Grate a b s t = Grate {runGrate :: (((s -> a) -> b) -> t)}
{-_Grate = promap runGrate Grate-}

instance Closed (Grate a b) where
  closed (Grate z) = Grate (\f x -> z (\k -> f (\g -> k (g x))))

instance Promap (Grate a b) where
  promap f g (Grate z) = Grate (\d -> g (z (\k -> d (\x -> k (f x)))))

withGrate :: (Grate a b a b -> Grate a b s t) -> ((s -> a) -> b) -> t
withGrate g = case g (Grate (\f -> f (\x -> x))) of Grate z -> z

{-type (s &~  a) b t = forall p. Closed p => p a b -> p s t-}
{-type  s &~~ a      = forall p. Closed p => p a a -> p s s-}

{-type (s &~.  a) b t = Grate a b a b -> Grate a b s t-}
{-type  s &~~. a      = Grate a a a a -> Grate a a s s-}

--withGrate :: (s &~. a) b t -> ((s -> a) -> b) -> t

{-cloneGrate :: (s &~. a) b t -> (s &~ a) b t-}
cloneGrate :: Closed p => (Grate a b a b -> Grate a b s t) -> p a b -> p s t
cloneGrate g = grate (withGrate g)

{-zipWithOf :: Grate a b s t -> (a -> a -> b) -> (s -> s -> t)-}
{-zipWithOf (Grate g) op s1 s2 = g (\get -> get s1 `op` get s2)-}

zipFOf' :: Map f => Grate a b s t -> (f a -> b) -> f s -> t
zipFOf' (Grate g) reduce fs = g (\get -> reduce (map get fs))

{-grate0 :: Grate a b a b-}
{-grate0 = Grate (\aab -> aab (\a -> a))-}

{-repGrate :: (Grate a b a b -> Grate a b s t) -> Grate a b s t-}
{-repGrate g = g grate0-}

{-zipFOf :: Map f => (Grate a b a b -> Grate a b s t) -> (f a -> b) -> f s -> t-}
{-zipFOf g reduce fs = g grate0 `runGrate` \get -> reduce (map get fs)-}


{-_Zip2 :: (FZip V2 a b -> FZip V2 s t) -> (a -> a -> b) -> s -> s -> t-}
{-_Zip2 z aab s s' = _FZip z (\(V2 a a') -> aab a a') (V2 s s')-}


newtype Zip2 a b = Zip2 {runZip2 :: a -> a -> b}
instance Closed Zip2 where closed (Zip2 z) = Zip2 (\xa xa' x -> z (xa x) (xa' x))
instance Promap Zip2 where promap f g (Zip2 z) = Zip2 (\a a' -> g (z (f a) (f a')))
instance Map (Zip2 a) where map = postmap

_Zip2 :: (Zip2 a b -> Zip2 s t) -> (a -> a -> b) -> s -> s -> t
_Zip2 = promap Zip2 runZip2

newtype FZip f a b = FZip {runFZip :: f a -> b}

instance Map f => Closed (FZip f) where
  closed (FZip fab) = FZip (\fxa x -> fab (map (\f -> f x) fxa))
instance Map f => Promap (FZip f) where
  promap f g (FZip fab) = FZip (promap (map f) g fab)
instance Map (FZip f a) where map f (FZip fab) = FZip (\fa -> f (fab fa))
instance Duplicate w => Compose (FZip w) where FZip f > FZip g = FZip (g < extend f)
instance Comonad w => Category (FZip w) where id = FZip fold_

_FZip :: (FZip f a b -> FZip f s t) -> (f a -> b) -> f s -> t
_FZip = promap FZip runFZip
