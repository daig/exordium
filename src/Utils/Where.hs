module Utils.Where (module Utils.Where, module X) where
import Where.Type as X

where'bimap :: (x -> a) -> (y -> b) -> Where x y -> Where a b
where'bimap f g = \case
    Here a -> Here (f a)
    There b -> There (g b)
    Nowhere -> Nowhere

where'map :: (x -> b) -> Where a x -> Where a b
where'map = where'bimap (\a -> a)

where'lmap :: _
where'lmap = (`where'bimap` (\b -> b))
