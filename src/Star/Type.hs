module Star.Type where
import Map.Di as X

newtype Star f a b = Star {runStar :: a -> f b}

{-instance Bind m => Compose (Star m) where Star f > Star g = Star (g <=< f)-}
{-instance Monad m => Category (Star m) where id = Star pure-}
instance Map f => Dimap (Star f) where dimap f g (Star s) = Star (dimap f (map g) s)
instance Map f => Map (Star f a) where map f (Star s) = Star (\a -> map f (s a))
instance Map f => MapR (Star f) where rmap yb (Star afy) = Star (\a -> yb `map` afy a)
instance ComapL (Star f) where colmap ax (Star xfb) = Star (\a -> xfb (ax a))
{--- TODO: move to DimapIso class-}
instance MapIso f => MapIso (Star f a) where mapIso f g (Star p) = Star (\a -> mapIso f g (p a))
