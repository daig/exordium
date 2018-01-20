module Utils.Bind (module Utils.Bind, module X) where
import Class.Bind as X

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
