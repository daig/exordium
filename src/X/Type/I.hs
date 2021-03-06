module X.Type.I where

-- | Identity type
newtype I a = I a

-- TODO: inline these into instances definition
i'map :: (a -> b) -> I a -> I b
f `i'map` I a = I (f a)

i'traverse_ :: (forall a b. (a -> b) -> f a -> f b) -> (x -> f r) -> I x -> f (I r)
i'traverse_ mymap = \f (I a) -> I `mymap` f a
i'foldMap_ :: (a -> b) -> I a -> b
f `i'foldMap_` I a = f a
i'apply :: I (a -> b) -> I a -> I b
I f `i'apply` I a = I (f a)
