module Bind (Bind(..),(>>=),apDefault,module X) where
import Map as X
import Apply as X
import Append
import Fun

class Apply m => Bind m where
  {-# minimal join | (=<<) #-}
  join :: m (m a) -> m a
  join = (id =<<)
  (=<<) :: (a -> m b) -> m a -> m b
  f =<< m = join (f $@ m)
  (!<<) :: m a -> m b -> m a
  ma !<< mb = (!@ mb) =<< ma

(>>=) :: Bind m => m a -> (a -> m b) -> m b
m >>= f = f =<< m
(=<=) :: Bind m => (b -> m c) -> (a -> m b) -> a -> m c
g =<= f = (g =<<) < f
(=>=) :: Bind m => (a -> m b) -> (b -> m c) -> a -> m c
f =>= g = g =<= f

apDefault :: Bind m => m (a -> b) -> m a -> m b
apDefault mf ma = mf >>= ($@ ma)

instance Bind [] where
  (=<<) f = \case
    [] -> []
    a:as -> f a `append` (f =<< as)
