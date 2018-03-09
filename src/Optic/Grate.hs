module Optic.Grate (module Optic.Grate, module X) where
import Closed as X
import Distribute.Internal

newtype Grate a b s t = Grate {runGrate :: (((s -> a) -> b) -> t)}
{-_Grate = promap runGrate Grate-}
{-collectOf :: (Star f a b -> Star f s t) -> (a -> f b) -> s -> f t-}
{-collectOf g f = case g (Star f) of Star f' -> f'-}

instance Closed (Grate a b) where
  closed (Grate z) = Grate (\f x -> z (\k -> f (\g -> k (g x))))

instance Promap (Grate a b) where
  promap f g (Grate z) = Grate (\d -> g (z (\k -> d (\x -> k (f x)))))
instance ComapL (Grate a b) where colmap = promap_colmap
instance MapR (Grate a b) where rmap = promap_rmap

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

zipWithOf :: Grate a b s t -> (a -> a -> b) -> (s -> s -> t)
zipWithOf (Grate g) op s1 s2 = g (\get -> get s1 `op` get s2)

zipFOf' :: Map f => Grate a b s t -> (f a -> b) -> f s -> t
zipFOf' (Grate g) reduce fs = g (\get -> reduce (map get fs))

grate0 :: Grate a b a b
grate0 = Grate (\aab -> aab (\a -> a))

repGrate :: (Grate a b a b -> Grate a b s t) -> Grate a b s t
repGrate g = g grate0

zipFOf :: Map f => (Grate a b a b -> Grate a b s t) -> (f a -> b) -> f s -> t
zipFOf g reduce fs = g grate0 `runGrate` \get -> reduce (map get fs)

newtype ZipF f a b  = ZipF {runZipF :: f a -> b}
instance Map f => Closed (ZipF f) where closed (ZipF fab) = ZipF (\fxa x -> fab (map (\f -> f x) fxa))
instance Map f => Promap (ZipF f) where promap f g (ZipF z) = ZipF (promap (map f) g z)
instance Map f => ComapL (ZipF f) where colmap f = promap f (\b -> b)
instance MapR (ZipF f) where rmap g (ZipF z) = ZipF (\fa -> g (z fa)) 
_ZipF :: (ZipF f a b -> ZipF f s t) -> (f a -> b) -> f s -> t
_ZipF = promap ZipF runZipF

_Zip2 :: (ZipF V2 a b -> ZipF V2 s t) -> (a -> a -> b) -> s -> s -> t
_Zip2 z aab s s' = _ZipF z (\(V2 a a') -> aab a a') (V2 s s')
