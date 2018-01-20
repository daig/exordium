module Utils.Bind (module Utils.Bind, module X) where
import Class.Bind as X

(>>=) :: Bind m => m a -> (a -> m b) -> m b
m >>= f = f =<< m
(=<=) :: Bind m => (b -> m c) -> (a -> m b) -> a -> m c
g =<= f = (g =<<) < f
(=>=) :: Bind m => (a -> m b) -> (b -> m c) -> a -> m c
f =>= g = g =<= f

(!<<) = constBind

bind_ap :: Bind m => m (a -> b) -> m a -> m b
bind_ap mf ma = mf >>= ($@ ma)
