module MapIso where

class MapIso f where mapIso :: (b -> a) -> (a -> b) -> f a -> f b

instance MapIso ((,) x) where mapIso _ f (x,a) = (x,f a)
instance MapIso ((->) x) where mapIso _ f g = \x -> f (g x)
instance MapIso [] where
  mapIso _ f = go where
    go = \case
      [] -> []
      a:as -> f a : go as
