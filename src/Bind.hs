module Bind (Bind(..),(>>=),apDefault,module X) where
import Map as X
import Apply as X

class Apply m => Bind m where
  {-# minimal join | bind #-}
  join :: m (m a) -> m a
  join = bind (\x -> x)
  bind :: (a -> m b) -> m a -> m b
  bind f m = join (map f m)
  constBind :: m a -> m b -> m a
  constBind ma mb = bind (\a -> constMap a mb) ma

(>>=) :: Bind m => m a -> (a -> m b) -> m b
m >>= f = bind f m

apDefault :: Bind m => m (a -> b) -> m a -> m b
apDefault mf ma = bind (\f -> map f ma) mf
