module Indexed where
import Iso as X
import AFold
import AReview

class Indexed f where
  {-# minimal indexed | index,tabulate #-}
  type Ix f :: *
  indexed :: f =:~ ((->) (Ix f))
  indexed = isoF index tabulate
  index :: f a -> Ix f -> a
  index = foldOf indexed
  tabulate :: (Ix f -> a) -> f a
  tabulate = review indexed
