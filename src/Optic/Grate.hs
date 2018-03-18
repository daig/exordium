module Optic.Grate (module Optic.Grate, module X) where
import Arrow.Mapped as X
import Arrow.Category as X
import Monad.Co as X
import Map.Co

import Distribute.Internal
import Adjoint

newtype Grate a b s t = Grate {runGrate :: (((s -> a) -> b) -> t)}
{-_Grate = promap runGrate Grate-}

instance Traversed (Grate a b)
instance Traversed1 (Grate a b)
instance Traversed0 (Grate a b)
instance Traversed_ (Grate a b) where
  {-lens sa sbt = Grate (\sa'b -> sa'b't (\-}
instance Traversed' (Grate a b)
instance Mapped (Grate a b)


instance Closed (Grate a b) where
  closed (Grate z) = Grate (\f x -> z (\k -> f (\g -> k (g x))))

instance Promap (Grate a b) where
  promap sa bt (Grate aabb) = Grate (\sab -> bt (aabb  (\aa -> sab (\s -> aa (sa s)))))

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
instance Pure (Zip2 a) where pure x = Zip2 (\_ _ -> x)
instance Apply (Zip2 a) where ap = zipF_ap
instance Applicative (Zip2 a) 
instance Distribute (Zip2 a) where zipF fab faab = Zip2 (\a b -> fab (map (\(Zip2 f) -> f a b) faab))
instance Closed Zip2 where closed (Zip2 z) = Zip2 (\xa xa' x -> z (xa x) (xa' x))
{-instance Traversed Zip2 where traversal afbsft (Zip2 z) = Zip2 (\s s' -> -}
{-instance Mapped Zip2 where setter abst (Zip2 z) = Zip2 (\s s' -> abst (\x -> z x x) s)-}
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
instance Duplicate w => Compose (FZip w) where FZip f `precompose` FZip g = FZip (g `postcompose` extend f)
instance Comonad w => Category (FZip w) where identity = FZip fold_

_FZip :: (FZip f a b -> FZip f s t) -> (f a -> b) -> f s -> t
_FZip = promap FZip runFZip


-- Bad
{-instance Mapped Zip2 where mapping afbsft (Zip2 aab) = Zip2 (\s _ -> afbsft (\a _ -> aab a a) s s)-}
{-instance Traversed Zip2 where traversal afbsft (Zip2 aab) = Zip2 (\s _ -> afbsft (\a _ -> aab a a) s s)-}
{-instance Traversed1 Zip2 where traversal1 = traversal-}
{-instance Traversed0 Zip2 where traversal0 = traversal-}
{-instance Traversed_ Zip2 where-}
  {-lens sa sbt (Zip2 aab) = Zip2 (\s s' -> sbt s (aab (sa s) (sa s')))-}
instance Traversed' Zip2 where
  prism seta bt (Zip2 aab) = Zip2 ff where
    ff s s' = case (seta s,seta s') of
      (L t, _) -> t
      (_,L t) -> t
      (R a, R a') -> bt (aab a a')



{-newtype Z f s t a b = Z {runZ :: (f s -> t) -> f a -> b}-}
{-instance Comap f => Promap (Z f s t) where-}
  {-promap ax yb (Z fstfxy) = Z (\fst fa -> -}
  {-promap ax yb (Z fxyfst) = Z (\fab fs -> fxyfst (\fx -> yb (fab (comap ax fx))) fs)-}
  
instance Traverse V2 where traverse = traverse1
instance Traverse1 V2 where traverse1 afb (V2 a b) = V2 `map` afb a `ap` afb b
instance Fold V2 where foldMap = traverse_foldMap
instance Fold1 V2 where foldMap1 = traverse1_foldMap1


newtype Optic f g a b = Optic {runOptic :: f a -> g b}
instance (Map f, Map g) => Promap (Optic f g) where
  promap f g (Optic fagb) = Optic (promap (map f) (map g) fagb)

{-instance (Pure f, Map g) => Traversed_ (Optic f g) where traversal_ afbsft (Optic fagb)-}
  {-= Optic (\fs -> afbsft (\a -> fagb (pure a)) fs)-}


{-instance Pure f => Traversed' (Traversing f) where-}
  {-prism pat constr (Traversing afb) = Traversing (\s -> case pat s of-}
    {-L t -> pure t-}
    {-R a -> constr `map` afb a)-}
{-instance Apply f => Traversed1 (Traversing f) where-}
  {-traversal1 afbsft (Traversing afb) = Traversing (\s -> afbsft afb s)-}
{-instance Pure f => Traversed0 (Traversing f) where-}
  {-traversal0 afbsft (Traversing afb) = Traversing (\s -> afbsft afb s)-}
{-instance Applicative f => Traversed (Traversing f) where-}
  {-traversal afbsft (Traversing afb) = Traversing (\s -> afbsft afb s)-}

