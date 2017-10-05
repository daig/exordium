module Permute where
{-import Lens-}
import Plus
import Ord

class Negate (Permutation t) => Permute t where
  type Permutation t
  permute :: Permutation t -> t -> t
  {-permuted :: Permutation f -> Iso' t t-}
class Permute f => Order f where
  order :: Ord a => f a -> Permutation f
class Sort f where
  sort :: Ord a => f a -> f a
  default sort :: (Permute f, Order f, Ord a) => f a -> f a
  sort f = permute (order f) f
class Reverse 
