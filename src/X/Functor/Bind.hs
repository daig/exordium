module X.Functor.Bind (module X.Functor.Bind, module X) where
import X.Functor.Apply as X
import X.Functor.Plus
import {-# source #-} X.Data.E

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
    a:as -> f a `fplus` (f `bind` as)

(=<<) :: Bind m => (a -> m b) -> m a -> m b
(=<<) = bind
(>>=) :: Bind m => m a -> (a -> m b) -> m b
m >>= f = f =<< m
(=<=) :: Bind m => (b -> m c) -> (a -> m b) -> a -> m c
g =<= f = \x -> g `bind` f x
(=>=) :: Bind m => (a -> m b) -> (b -> m c) -> a -> m c
f =>= g = g =<= f

(!<<) :: Bind m => m a -> m b -> m a
(!<<) = constBind

(>>!) :: Bind m => m a -> m b -> m b
a >>! b = b !<< a

bind_ap :: Bind m => m (a -> b) -> m a -> m b
bind_ap mf ma = (`map` ma) `bind` mf
instance Bind (E x) where
  join = \case
    L x -> L x
    R (L x) -> L x
    R (R x) -> R x
instance Apply (E x) where
