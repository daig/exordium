module Utils.I (module Utils.I, module X) where
import Type.I as X

f `i'map` I a = I (f a)
i'traverse_ map = \f (I a) -> I `map` f a
f `i'foldMap_` I a = f a
I f `i'apply` I a = I (f a)
