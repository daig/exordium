{-# language UnboxedSums #-}
module MapM.Class (module MapM.Class, module X) where
import Map as X
import Bool.Type as X
import Maybe
import Pure
import Empty
import List

true _ t = t
false f _ = f

-- TODO: add a quasiquoter for church encoding
-- like mapM' :: (a -> [t|() + b|]) -> f a -> f b
class Map f => MapM f where
  {-# minimal map' | filter #-}
  map' :: (forall r. a -> r -> (b -> r) -> r) -> f a -> f b
  map' f x = map (\case Just t -> t)
    (filter (\case {Nothing -> false; _ -> true}) 
    ( map (\a -> f a Nothing Just) x))
  {-mapM f x = map (\case Some a -> a)-}
                {-( filter (\case {None -> False; _ -> True})-}
                {-( map f x))-}
  filter :: (forall r. a -> r -> r -> r) -> f a -> f a
  filter p =  map' (\a r ar -> p a r (ar a))


instance MapM [] where
  filter = list'filter
  {-map' = list'map'-}


map'_map :: MapM f => (a -> b) -> f a -> f b
map'_map f = map' (\a _ br -> br (f a))

test :: [Bool] -> [Bool]
test = map' (\x a b -> case x of {True -> b True; False -> a})
{-list'mapM = mapM @[] (\case {True -> Some True; False -> None})-}

{-data Option a = None | Some a-}
