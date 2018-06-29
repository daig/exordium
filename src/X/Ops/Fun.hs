module X.Ops.Fun where

f < g = \a -> f (g a)
f $! x = let !vx = x in f vx 
infixr 0 $!


error s = let x = x in x -- TODO: fix and move to debug
