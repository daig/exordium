module MapMaybe where
import Map
import Bool
import Maybe

class Map f => MapMaybe f where
  {-# minimal mapMaybe | filter #-}
  mapMaybe :: (a -> Maybe b) -> f a -> f b
  mapMaybe f x = map fromJust (filter isJust (map f x))
  filter :: (a -> Bool) -> f a -> f a
  filter f x =  mapMaybe (\a -> case f a of {False -> Nothing; True -> Just a}) x

-- mapMaybe Just = id
-- mapMaybe (f <=< g) = mapMaybe f . mapMaybe g

fromJust :: Maybe a -> a
fromJust = \case
  Just a -> a
isJust :: Maybe a -> Bool
isJust = \case
  Nothing -> False
  Just _ -> True

mapDefault :: MapMaybe f => (a -> b) -> f a -> f b
mapDefault f = mapMaybe (\a -> Just (f a))
