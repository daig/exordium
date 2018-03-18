module Where (Where(..)) where
import Functor.Bipure
import Functor.Align
import Functor.Pure

data Where a b = Here a | There b | Nowhere

instance Bimap Where where bimap = where'bimap
where'bimap :: (x -> a) -> (y -> b) -> Where x y -> Where a b
where'bimap f g = \case
    Here a -> Here (f a)
    There b -> There (g b)
    Nowhere -> Nowhere

instance Map (Where a) where map = where'map
where'map :: (x -> b) -> Where a x -> Where a b
where'map = where'bimap (\a -> a)


instance Bipure Where where
  purel = Here
  purer = There

instance Choose (Where a) where
  choose bfc = \case
    Here a -> pure (Here a)
    There b -> There `map` bfc b
    Nowhere -> pure Nowhere
class Choose t where choose :: (Align f,Pure f) => (a -> f b) -> t a -> f (t b)
