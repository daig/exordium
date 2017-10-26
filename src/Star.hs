module Star
  (Star(..)
  ,module X) where
import Category as X
import Monad as X (Monad)
import Monad
import Dimap as X

newtype Star f a b = Star {runStar :: a -> f b}
instance Bind m => Compose (Star m) where Star f > Star g = Star (g <=< f)
instance Monad m => Category (Star m) where id = Star pure
instance Map f => Dimap (Star f) where dimap f g (Star s) = Star (dimap f (map g) s)
instance Map f => Map (Star f a) where map = postmap
