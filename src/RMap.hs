module RMap (module RMap, module X) where
import RMap.Class as X
import Map as X

rmap_map :: RMap p => (x -> b) -> p a x -> p a b
rmap_map = rmap
