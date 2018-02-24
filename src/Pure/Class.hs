module Pure.Class (module Pure.Class, module X) where
import Map.Class as X
import Lifts.Class as X
import Zero.Class as X
import {-# source #-} E as X
import {-# source #-} K
import Any
import Bimap.Class

-- http://r6research.livejournal.com/28338.html
-- a Pure f is strong with respect to E

-- | Natural laws:
-- distR < right (map f) = map (right f) < distR
-- distR < left f = map (left f) < distR
-- 
-- Derived Laws:
-- distR < L = pure < L
-- dirtR < R = map R
class Map f => Pure f where
  {-# minimal pure | point | distL | distR #-}
  pure :: a -> f a
  pure a = a `constMap` point
  distL :: E (f a) b -> f (E a b)
  distL = \case {L a -> map L a; R b -> map R (pure b)}
  distR :: E a (f b) -> f (E a b)
  {-distR e = map e'swap (distL (e'swap e)) where e'swap = \case {L a -> R a; R b -> L b}-}
  point :: f ()
  point = map (\case {L a' -> a'; R r -> case r of {}}) (distR (L @() @(f Any) ()))

{-f i (t b) -> t (f i b)-}
{-distR' = \case {L a -> map L (pure a); R b -> map R b}-}
{-distR' = bimap (\a -> map L (pure a)) (map R)-}

pure_zero :: (Pure f, Zero a) => f a
pure_zero = pure zero

instance Pure ((->) x) where pure a = \_ -> a
instance Pure [] where pure a = [a]
instance Zero a => Pure (K a) where pure _ = zero

q = \case
  L a -> map L a
  R b -> map R b
{-type X f = (Pure f, -}
{-{-class Sum t where-}-}
  {-distribute_ :: Map f => f (t a) -> t (f a)-}
  {-distribute_ = collect (\x -> x)-}
   {-aka cotraverse-}
  {-zipF_ :: Map f => (f a -> b) -> f (t a) -> t b-}
  {-zipF_ f = \fta -> map f (distribute fta)-}
  {-collect_ :: Map f => (a -> t b) -> f a -> t (f b)-}
  {-collect_ f a  = zipF (\x -> x) (map f a)-}
{-class (Traverse0 t, Traverse1 t,FoldMap_ t) => Traverse_ t where-}
  {-traverse_ :: Map f => (a -> f b) -> t a -> f (t b)-}
  {-traverse_ f t = sequence_ (map f t)-}
  {-sequence_ :: Map f => t (f a) -> f (t a)-}
  {-sequence_ = traverse_ (\x -> x)-}
{-class Applicative t => Distribute t where-}
  {-{-# minimal distribute | collect | zipF #-}-}
  {-distribute :: Map f => f (t a) -> t (f a)-}
  {-distribute = collect (\x -> x)-}
  -- aka cotraverse
  {-zipF :: Map f => (f a -> b) -> f (t a) -> t b-}
  {-zipF f = \fta -> map f (distribute fta)-}
  {-collect :: Map f => (a -> t b) -> f a -> t (f b)-}
  {-collect f a  = zipF (\x -> x) (map f a)-}
