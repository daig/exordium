module List where

list'map :: (a -> b) -> [a] -> [b]
list'map f = go where
  go = \case
    [] -> []
    a:as -> f a : go as
{-# noinline list'map #-}

list'foldMap :: acc -> (x -> acc -> acc) -> (a -> x) -> [a] -> acc
list'foldMap zero add = go' where
  go' f = go where
    go = \case
      [] -> zero
      a:as -> f a `add` go as

list'traverse :: (forall a b. (a -> b) -> f a -> f b)
              -> (forall a. a -> f a)
              -> (forall a b. f (a -> b) -> f a -> f b)
              -> (x -> f y) -> [x] -> f [y]
list'traverse map pure ap = go' where
  go' f = go where
    go = \case
      [] -> pure []
      (x:xs) -> (:) `map` f x  `ap` go xs

list'prepend,list'append :: [a] -> [a] -> [a]
list'prepend bs = go where
  go = \case
    [] -> bs
    a:as -> a : go as
list'append as bs = list'prepend bs as

list'filter :: (forall r. a -> r -> r -> r) -> [a] -> [a]
list'filter p = go where
  go = \case
    [] -> []
    a:as -> p a as (a:as)
{-# noinline[0] list'filter #-}

list'map' :: (forall r. a -> r -> (b -> r) -> r) -> [a] -> [b]
list'map' amb = go where
  go = \case
    [] -> [] 
    a:as -> amb a (go as) (\b -> b:go as)
{-# noinline list'map' #-}


{-q :: (a -> [b] -> [b] -> [b]) -> (a -> b) -> [a] -> [b]-}
filter_map :: (forall r. b -> r -> r -> r) -> (a -> b) -> [a] -> [b]
filter_map p f = list'map' (\a n j -> let b = f a in p b n (j b))
map_filter :: (forall r. a -> r -> r -> r) -> (a -> b) -> [a] -> [b]
map_filter p f = list'map' (\a n j -> p a n (j (f a)))

{-map_map' p f as = list'map f (list'map' p as)-}

map'_map :: (forall r. x -> r -> (b -> r) -> r) -> (a -> x) -> [a] -> [b]
map'_map p f as = list'map' (\a n j -> p (f a) n j) as
{-# inline map'_map #-}


{-a = 'a'-}
{-# RULES
"list'map/filter" forall (p :: forall r. a -> r -> r -> r) (f :: a -> b) (as :: [a]).
 list'map f (list'filter p as) = map_filter p f as
"list'filter/map" forall (p :: forall r. b -> r -> r -> r) (f :: a -> b) (as :: [a]).
 list'filter p (list'map f as) = filter_map p f as
"list'map'/map" forall (p :: forall r. x -> r -> (b -> r) -> r) (f :: a -> x) (as :: [a]).
 list'map' p (list'map f as) = list'map' (\a n j -> p (f a) n j) as
#-}
