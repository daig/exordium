module I where
import Applicative.Class
import Distribute.Class
import FoldMap_.Class

-- | Identity type
newtype I a = I a

f `i'map` I a = I (f a)
i'traverse_ map = \f (I a) -> I `map` f a
f `i'foldMap_` I a = f a
I f `i'apply` I a = I (f a)

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
