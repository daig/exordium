module MapF (MapF(..), BimapF(..), mapfDefault, ($@@), module X) where
import Map as X
import Dimap as X
import NatTrans as X
import Constraint as X

class MapF t where
  type MapFC t :: (* -> *) -> Constraint -- TODO: unsatisfied with name
  type MapFC t = Map -- TODO: merge with other transformer alass constraints?
  mapf :: MapFC t f => (f --> g) -> t f a -> t g a
($@@) :: (MapF t, MapFC t f) => (f --> g) -> t f a -> t g a
($@@) = mapf
class BimapF t where 
  type LMapC t :: (* -> *) -> Constraint
  type LMapC t = Map
  type RMapC t :: (* -> *) -> Constraint
  type RMapC t = Map
  bimapf :: (LMapC t f, RMapC t g) => (f --> f') -> (g --> g') -> t f g a -> t f' g' a
mapfDefault :: (BimapF t,LMapC t f, RMapC t g) => (g --> g') -> t f g a -> t f g' a
mapfDefault = bimapf (\x -> x)
