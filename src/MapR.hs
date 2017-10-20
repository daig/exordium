module MapR where

class MapR p where mapr :: (x -> b) -> p a x -> p a b

instance MapR (,) where mapr f (a,x) = (a, f x)
instance MapR (->) where mapr f g = \x -> f (g x)
