module Type.Star (module Type.Star, module X) where
import Category as X
import Class.Monad as X (Monad)
import Class.Monad
import Class.Dimap as X

newtype Star f a b = Star {runStar :: a -> f b}
{-instance Bind m => Compose (Star m) where Star f > Star g = Star (g <=< f)-}
{-instance Monad m => Category (Star m) where id = Star pure-}
{-instance Map f => Dimap (Star f) where dimap f g (Star s) = Star (dimap f (map g) s)-}
{-instance Map f => Map (Star f a) where map = postmap-}
{--- TODO: move to DimapIso class-}
{-instance MapIso f => MapIso (Star f a) where mapIso f g (Star p) = Star (\a -> mapIso f g (p a))-}
