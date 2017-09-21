module Embed where
import Map
import Hoist

class Hoist t => Embed t where
  {-# minimal embed | squash #-}
  embed :: Map g => (forall x. f x -> t g x) -> t f a -> t g a
  embed f t = squash (hoist f t)
  squash :: Map f => t (t f) a -> t f a
  squash = embed (\x -> x)
