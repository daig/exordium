module I where
import Applicative.Class

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
