{-# language UnboxedSums #-}
module X.Functor.Map' (module X.Functor.Map', module X) where
import X.Functor.HMap as X
import X.Data.Bool as X
import X.Data.Maybe
import X.Data.X



-- TODO: add a quasiquoter for church encoding
-- like map' :: (a -> [t|() + b|]) -> f a -> f b

class Map f => Map' f where
  {-# minimal map' | filter #-}
  map' :: (a -> Maybe b) -> f a -> f b
  map' f x = map (\a -> case f a of {Just b -> b; Nothing -> __})
    (filter (\a -> case f a of {Nothing -> F; Just{} -> T}) x)
  filter :: (a -> Bool) -> f a -> f a
  filter p =  map' (\a -> if p a then Just a else Nothing)


instance Map' [] where
  map' f = go where
    go = \case
      [] -> []
      a:as -> case f a of {Nothing -> go as; Just b -> b : go as}
instance Map' f => EMap () f where emap f = map' (\a -> case f a of {L () -> Nothing; R a -> Just a})

map'_map :: Map' f => (a -> b) -> f a -> f b
map'_map f = map' (\a -> Just (f a))
