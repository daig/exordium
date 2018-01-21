module MapM (module MapM, module X) where
import MapM.Class as X

mapM_map :: MapM f => (a -> b) -> f a -> f b
mapM_map f = mapM (\a -> Just (f a))
