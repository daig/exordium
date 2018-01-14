module Indexed
  (Indexed(..)
  ,mapDefault
  ,module X) where
import Isos as X
import Distributive as X
import AFold (foldOf)
import AReview (review)
import ASetter (over)
import Apply

class Indexed f where
  {-# minimal indexed | index,tabulate #-}
  type Ix f :: *
  indexed :: f ~~= ((->) (Ix f))
  indexed = isoF index tabulate
  index :: f a -> Ix f -> a
  index = foldOf indexed
  tabulate :: (Ix f -> a) -> f a
  tabulate = review indexed

{-mapDefault :: Indexed f => (a -> b) -> f a -> f b-}
{-mapDefault f = over indexed (map f)-}

distributeDefault :: (Indexed i, Map f) => f (i a) -> i (f a)
distributeDefault fi = tabulate (\k -> map (`index` k) fi)

apDefault :: Indexed f => f (a -> b) -> f a -> f b
apDefault f g = tabulate (index f |$| index g)

instance Indexed ((->) x) where
  type Ix ((->) x) = x
  indexed = (\x -> x)
