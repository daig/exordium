{-# language MagicHash #-}
module Map.Class (module Map.Class, module X) where
import Coerce
import Map.Iso as X
import {-# source #-} K
import List
import Tuple
import FPlus.Class
import Coerce
import Prelude (error,String)

class MapIso f => Map (f :: * -> *) where
  map :: (a -> b) -> f a -> f b
  -- | Try to coerce if @f@ is parametric.
  map# :: a #= b => (a -> b) -> f a -> f b
  map# f !x = map f x
  -- | Inform the outer @g@ whether @f@ is parametric.
  -- This is an ugly hack to get correct behavior for @O f g@.
  -- TODO: see if there's a better way, or if its even necessary
  map## :: (Map g, a #= b) => (a -> b) -> g (f a) -> g (f b)
  map## f !x = map (map# f) x
  constMap :: b -> f a -> f b
  constMap b = map (\_ -> b)

{-map# :: (a #= b, Map f) => (a -> b) -> f a -> f b-}
{-map# f = para coerce (map f)-}
map_map# f !x = map f x

para'map## :: (Map f, Map g, f a #= f b, a #= b) => (a -> b) -> g (f a) -> g (f b)
para'map## _ = map# coerce

instance Map ((->) x) where
  map f p = \a -> f (p a)
  map# _ = coerce
  map## _ = map# coerce
instance Map [] where map = list'map
instance Map ((,) x) where map = tuple'map
instance Map (K a) where map _ = coerce
