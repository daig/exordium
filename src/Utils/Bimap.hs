module Utils.Bimap (module Utils.Bimap, module X) where
import Bimap.Class as X

bimap_lmap :: Bimap p => (x -> a) -> p x b -> p a b
bimap_lmap = (`bimap` (\b -> b))
bimap_rmap :: Bimap p => (x -> b) -> p a x -> p a b
bimap_rmap = bimap (\a -> a)
