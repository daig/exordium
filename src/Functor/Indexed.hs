module Functor.Indexed
  (module Functor.Indexed
  ,module X) where
import Arrow.Promap as X
import Functor.Distribute as X
import Optic.Review
import Optic.View
import Arrow.Promap
import Functor.Apply

class Distribute f => Indexed f where
  {-# minimal indexed | index,tabulate #-}
  type Ix f :: *
  indexed :: f ~~= ((->) (Ix f))
  indexed = isoF index tabulate
  index :: f a -> Ix f -> a
  index = view indexed
  tabulate :: (Ix f -> a) -> f a
  tabulate = _Review indexed

indexed_map :: Indexed f => (a -> b) -> f a -> f b
indexed_map f = indexed (map f)

indexed_distribute :: (Indexed i, Map f) => f (i a) -> i (f a)
indexed_distribute fi = tabulate (\k -> map (`index` k) fi)

indexed_ap :: Indexed f => f (a -> b) -> f a -> f b
indexed_ap f g = tabulate (index f `ap` index g)

instance Indexed ((->) x) where
  type Ix ((->) x) = x
  indexed = (\x -> x)
