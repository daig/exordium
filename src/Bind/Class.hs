module Bind.Class (module Bind.Class, module X) where
import Map.Class as X
import Apply.Class as X
import Utils.Fun
import Utils.List

-- | Associativity of join:
--  join < join = join < map join
--  or equivalently
-- m >>= (\x -> k x >>= h) = (m >>= k) >>= h
--  or equivalently (f >=> (g >=> h)) = ((f >=> g) >=> h)
--
-- Distribution over |$|:
-- join (f |$(|$|)$| a) = join f |$| join a
class Apply m => Bind m where
  {-# minimal join | bind #-}
  join :: m (m a) -> m a
  join = bind (\x -> x)
  bind :: (a -> m b) -> m a -> m b
  f `bind` m = join (f `map` m)
  constBind :: m a -> m b -> m a
  ma `constBind` mb = (`constMap` mb) `bind` ma


instance Bind ((->) r) where
  f `bind` g = \r -> f (g r) r
instance Bind [] where
  bind f = \case
    [] -> []
    a:as -> f a `list'append` (f `bind` as)
