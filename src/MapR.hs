module MapR where
import Witness as X
import Map as X

class MapR p where
  mapr :: (x -> b) -> p a x -> p a b
  mapDict :: forall i. W (Map (p i))
  default mapDict :: Map (p i) => W (Map (p i)) -- TODO: Why doesn't this work??
  mapDict = W

instance MapR (,) where mapr f (a,x) = (a, f x); mapDict = W
instance MapR (->) where mapr f g = \x -> f (g x); mapDict = W
