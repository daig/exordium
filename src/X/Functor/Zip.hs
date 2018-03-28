{-# language MagicHash #-}
module X.Functor.Zip (module X.Functor.Zip, module X) where
import X.Functor.Zip.Internal
import X.Functor.Map
import {-# source #-} X.Type.I
import {-# source #-} X.Type.K
import X.Functor.Fold
import X.Functor.Applicative as X

-- TODO: is Applicative right? or should we have a distinct Zip class that handles day convolution
class Applicative t => Zip t where
  {-# minimal distribute | collect | zip #-}
  distribute :: Map f => f (t a) -> t (f a)
  distribute = collect (\x -> x)
  -- aka cotraverse
  zip :: Map f => (f a -> b) -> f (t a) -> t b
  zip f = \fta -> map f (distribute fta)
  collect :: Map f => (a -> t b) -> f a -> t (f b)
  collect f a  = zip (\x -> x) (map f a)


-- TODO: merge into data family
zip2 :: Zip t => (a -> a -> b) -> t a -> t a -> t b
zip2 f t t' = zip (\(V2 a b) -> f a b) (V2 t t')

-- TODO: Avoid incomplete pattern
zip_zip :: Zip t => (a -> b -> r) -> t a -> t b -> t r
zip_zip f t t' = zip (\(V2 (L a) (R b)) -> f a b) (V2 (map L t) (map R t'))

zip_ap :: Zip t => t (a -> r) -> t a -> t r
zip_ap = zip_zip (\f -> f)

collect_map :: Zip t => (a -> b) -> t a -> t b
collect_map f ta = case collect (\x -> I (f x)) ta of I tb -> tb

distribute_distR :: Zip t => E x (t a) -> t (E x a)
distribute_distR = distribute

distribute_pure :: forall t a. Zip t => a -> t a
distribute_pure a = map (\(K x) -> x) (distribute (K a))

{-distribute_empty :: forall t a. Zip t => t a-}
{-distribute_empty = map (\(K x) -> absurd x) (distribute loop)-}
  {-where loop = loop-}

instance Zip I where distribute a = I (map fold_ a)
instance Zip ((->) x) where
  collect axb fa = \x -> (\a -> axb a x) `map` fa
  distribute fxa = \x -> (\f -> f x) `map` fxa
