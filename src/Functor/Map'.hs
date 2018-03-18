{-# language UnboxedSums #-}
module Functor.Map' (module Functor.Map', module X) where
import Functor.Map as X
import Bool as X
import Maybe
import Functor.Pure
import Functor.Empty

true _ t = t
false f _ = f

-- TODO: add a quasiquoter for church encoding
-- like mapM' :: (a -> [t|() + b|]) -> f a -> f b
class Map f => Map' f where
  {-# minimal map' | filter #-}
  map' :: (forall r. a -> r -> (b -> r) -> r) -> f a -> f b
  map' f x = map (\case Just t -> t)
    (filter (\case {Nothing -> false; _ -> true}) 
    ( map (\a -> f a Nothing Just) x))
  {-mapM f x = map (\case Some a -> a)-}
                {-( filter (\case {None -> F; _ -> T})-}
                {-( map f x))-}
  filter :: (forall r. a -> r -> r -> r) -> f a -> f a
  filter p =  map' (\a r ar -> p a r (ar a))


instance Map' [] where
  filter = list'filter
  {-map' = list'map'-}

list'filter :: (forall r. a -> r -> r -> r) -> [a] -> [a]
list'filter p = go where
  go = \case
    [] -> []
    a:as -> p a as (a:as)
{-# noinline[0] list'filter #-}


map'_map :: Map' f => (a -> b) -> f a -> f b
map'_map f = map' (\a _ br -> br (f a))

test :: [Bool] -> [Bool]
test = map' (\x a b -> case x of {T -> b T; F -> a})
{-list'mapM = mapM @[] (\case {T -> Some T; F -> None})-}

{-data Option a = None | Some a-}


{-filterJust :: MapM f => f (Maybe a) -> f a-}
{-filterJust = mapM (\x -> x)-}
