module X.Ops.Fun where

f > g = \a -> g (f a)
f < g = \a -> f (g a)
f $! x = let !vx = x in f vx 
infixr 0 $!


