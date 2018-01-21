module List where

list'map :: (a -> b) -> [a] -> [b]
list'map f = go where
  go = \case
    [] -> []
    a:as -> f a : go as

list'foldMap zero plus = go' where
  go' f = go where
    go = \case
      [] -> zero
      a:as -> f a `plus` go as

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

list'append [] bs = bs
list'append (a:as) bs = a : list'append as bs
