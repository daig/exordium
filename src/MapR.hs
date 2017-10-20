module MapR where

class MapR p where mapR :: (x -> b) -> p a x -> p a b

instance MapR (,) where mapR f (a,x) = (a, f x)
instance MapR (->) where mapR f g = \x -> f (g x)
