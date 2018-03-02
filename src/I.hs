module I where
import Applicative.Class
import Distribute.Class
import FoldMap_.Class

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

instance One a => One (I a) where one = I one
instance Applicative I
instance Apply I where I f `ap` I a = I (f a)
instance Pure I where pure = I
instance Map I where map = i'map
instance MapIso I where mapIso _ = i'map

instance Distribute I where distribute a = I (map fold_ a)
instance FoldMap_ I where foldMap_ = i'foldMap_
instance FoldMap0 I where foldMap0 = i'foldMap_
instance FoldMap1 I where foldMap1 = i'foldMap_
instance FoldMap I where foldMap = i'foldMap_
