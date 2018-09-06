module X.Functor.FDivide (module X.Functor.FDivide, module X) where
import X.Functor.FTimes as X
import X.Functor.Comap as X

class (Comap f, FTimes f) => FDivide f where
  fdivide :: (x -> a) -> (x -> b) -> f a -> f b -> f x
  fdivide f g fa fb = comap (\x -> (f x, g x)) (ftimes fa fb)

fdivide_comap :: FDivide f => (b -> a) -> f a -> f b -- TODO: is this a law-abiding implementation?
fdivide_comap f fa = fdivide f f fa fa

fdivide_ftimes :: FDivide f => f a -> f b -> f (a,b)
fdivide_ftimes = fdivide (\(a,_) -> a) (\(_,b) -> b)
