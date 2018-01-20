module Utils.MapM (module Utils.MapM, module X) where
import Class.MapM as X

mapM_map :: MapM f => (a -> b) -> f a -> f b
mapM_map f = mapM (\a -> Just (f a))
