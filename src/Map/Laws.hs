module Map.Laws where
import Map
import Bool

mapId :: (Map f, MapC f a, Eq (f a)) => f a -> Bool
mapId a = map (\x -> x) a == a
distrib :: (Map f, MapC f a, MapC f b, MapC f c, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
distrib f g a = map (\x -> g (f x)) a == map g (map f a)
