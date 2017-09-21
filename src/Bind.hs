module Bind where
import Map
import Apply

class Apply m => Bind m where
  {-# minimal join | bind #-}
  join :: m (m a) -> m a
  join = bind (\x -> x)
  bind :: (a -> m b) -> m a -> m b
  bind f m = join (map f m)

(>>=) :: Bind m => m a -> (a -> m b) -> m b
m >>= f = bind f m


