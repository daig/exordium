module Zip where
import Map
import Plus
import Append
import Empty
import Times
import Bool
import Swap

class Map f => Zip f where
  {-# minimal (|*|) | zipWith #-}
-- | Primarily used with lazy data
  (|*|) :: f a -> f b -> f (a,b)
  (|*|) = zipWith (,)
-- | Primarily used with strict data
  zipWith :: (a -> b -> c) -> f a -> f b -> f c
  zipWith f a b = map (\(x,y) -> f x y)  (a |*| b)

{--- Laws-}
commutes :: (Zip f, Eq (f (a,b))) => f a -> f b -> Bool
commutes fa fb = (fa |*| fb) == map swap (fb |*| fa)
zipEmpty :: forall f a b. (Empty f, Zip f, Eq (f (a,b))) => f a -> Bool
zipEmpty fa = (fa |*| empty @f @b) == empty
{-zipPure :: (Pure f, Zip f, Eq (f (a, b))) => f b -> a -> Bool-}

