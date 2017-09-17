module TimesF where
import Map
import Bool


class TimesF f where (|*|) :: f a -> f a -> f a
assoc :: (Eq (f a), TimesF f) => f a -> f a -> f a -> Bool
assoc a b c = (a|*|b)|*|c == a|*|(b|*|c)

class (Map f, TimesF f) => Pure f where
  pure :: a -> f a
  pure a = constMap a point
  point :: f ()
  point = pure ()

mapPure :: forall f a b. (Eq (f b), Map f, Pure f) => (a -> b) -> a -> Bool
mapPure f a = map f (pure a) == pure @f (f a)
timesPure :: (Eq (f a), TimesF f, Pure f) => f a -> a -> Bool
timesPure fa a = (fa |*| pure a) == fa && (pure a |*| fa) == fa
