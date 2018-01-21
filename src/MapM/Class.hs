module MapM.Class (MapM(..), module X) where
import Map.Class as X
import Type.Bool as X
import Type.Maybe as X

class Map f => MapM f where
  {-# minimal mapM | filter #-}
  mapM :: (a -> Maybe b) -> f a -> f b
  mapM f x = map (\case Just a -> a)
                ( filter (\case {Nothing -> False; _ -> True})
                ( map f x))
  filter :: (a -> Bool) -> f a -> f a
  filter f x =  mapM (\a -> case f a of {False -> Nothing; True -> Just a}) x
