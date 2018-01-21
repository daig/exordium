module Utils.RMap (module Utils.RMap, module X) where
import RMap.Class as X
import Utils.Map as X

rmap_map :: RMap p => (x -> b) -> p a x -> p a b
rmap_map = rmap
