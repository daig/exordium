module Apply where
import Map


class Map f => Apply f where
  {-# minimal (|@|) #-}
  (|@|) :: f (a -> b) -> f a -> f b
  (>>) :: f a -> f b -> f b
  a >> b = map (\x _ -> x) b |@| a

liftA2 :: Apply f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f x = (map f x |@|)

(|@) :: Apply f => f a -> (a -> b -> c) -> f b -> f c
fa |@ f = (map f fa |@|)
(@|) :: (f b -> f c) -> f b -> f c
f @| fb = f fb
{-assoc :: (Eq (f a), Apply f) => f a -> f a -> f a -> Bool-}
{-assoc a b c = (a|@|b)|@|c == a|@|(b|@|c)-}


{-mapPure :: forall f a b. (Eq (f b), Map f, Pure f) => (a -> b) -> a -> Bool-}
{-mapPure f a = map f (pure a) == pure @f (f a)-}
{-timesPure :: (Eq (f a), Apply f, Pure f) => f a -> a -> Bool-}
{-timesPure fa a = (fa |@| pure a) == fa && (pure a |@| fa) == fa-}
