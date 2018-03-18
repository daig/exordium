{-# language MagicHash #-}
module Functor.Distribute (module Functor.Distribute, module X) where
import Functor.Distribute.Internal
import Functor.Map
import {-# source #-} Type.I
import {-# source #-} Type.K
import Coerce (mapCoerce#)
import Functor.Fold
import Functor.Applicative as X

-- TODO: is Applicative right? or should we have a distinct Zip class that handles day convolution
class Applicative t => Distribute t where
  {-# minimal distribute | collect | zipF #-}
  distribute :: Map f => f (t a) -> t (f a)
  distribute = collect (\x -> x)
  -- aka cotraverse
  zipF :: Map f => (f a -> b) -> f (t a) -> t b
  zipF f = \fta -> map f (distribute fta)
  collect :: Map f => (a -> t b) -> f a -> t (f b)
  collect f a  = zipF (\x -> x) (map f a)


-- TODO: merge into data family
zip' :: Distribute t => (a -> a -> b) -> t a -> t a -> t b
zip' f t t' = zipF (\(V2 a b) -> f a b) (V2 t t')

-- TODO: Avoid incomplete pattern
zipF_zip :: Distribute t => (a -> b -> r) -> t a -> t b -> t r
zipF_zip f t t' = zipF (\(V2 (L a) (R b)) -> f a b) (V2 (map L t) (map R t'))

zipF_ap :: Distribute t => t (a -> r) -> t a -> t r
zipF_ap = zipF_zip (\f -> f)

collect_map :: Distribute t => (a -> b) -> t a -> t b
collect_map f ta = case collect (\x -> I (f x)) ta of I tb -> tb

distribute_distR :: Distribute t => E x (t a) -> t (E x a)
distribute_distR = distribute

distribute_pure :: forall t a. Distribute t => a -> t a
distribute_pure a = map (\(K x) -> x) (distribute (K a))

{-distribute_empty :: forall t a. Distribute t => t a-}
{-distribute_empty = map (\(K x) -> absurd x) (distribute loop)-}
  {-where loop = loop-}

instance Distribute I where distribute a = I (map fold_ a)
instance Distribute ((->) x) where
  collect axb fa = \x -> (\a -> axb a x) `map` fa
  distribute fxa = \x -> (\f -> f x) `map` fxa
