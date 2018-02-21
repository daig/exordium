module Where where
import InLR.Class

data Where a b = Here a | There b | Nowhere

instance Bimap Where where bimap = where'bimap
where'bimap :: (x -> a) -> (y -> b) -> Where x y -> Where a b
where'bimap f g = \case
    Here a -> Here (f a)
    There b -> There (g b)
    Nowhere -> Nowhere

instance MapIso (Where a) where mapIso _ = where'map
instance RMap Where where rmap = where'map
instance Map (Where a) where map = where'map
where'map :: (x -> b) -> Where a x -> Where a b
where'map = where'bimap (\a -> a)

instance LMap Where where lmap = where'lmap
where'lmap :: _
where'lmap = (`where'bimap` (\b -> b))

instance InLR Where where
  inL = Here
  inR = There
