module X.Arrow.Identity where

class Identity p where identity :: p a a

instance Identity (->) where identity = \x -> x
