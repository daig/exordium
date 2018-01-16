{-# language UndecidableInstances #-}
{-# language MagicHash #-}
module Map (module Map, module X) where
import Prelude (($)) -- TOOD: reexport
import MapIso as X
import Void
import Coerce

class MapIso f => Map f where
  {-# minimal map #-}
  map :: (a -> b) -> f a -> f b
  (!@) :: b -> f a -> f b
  (!@) b = map (\_ -> b)
map_mapIso :: Map f => (b -> a) -> (a -> b) -> f a -> f b
map_mapIso _ = map

($@) :: Map f => (a -> b) -> f a -> f b
($@) = map
(@$) :: Map f => f (a -> b) -> a -> f b
fab @$ a = ($ a) $@ fab
infixl 1 @$

instance Map ((,) x) where map = mapIso __
instance Map ((->) x) where map = mapIso __ 
instance Map [] where map = mapIso __

map# :: forall b a f. (Map f, a #= b) => (a -> b) -> f a -> f b
map# _ = coerce#
