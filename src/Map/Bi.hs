module Map.Bi (module Map.Bi, module X) where
import Map.L as X
import Map.R as X
import {-# source #-} E

-- | Independently Map each on both sides
class (MapL p, MapR p) => Bimap p where
  bimap :: (x -> a) -> (y -> b) -> p x y -> p a b
  bimap f g p = lmap f (rmap g p)
  {-bimap f g p = case map f (Flip (map g p)) of Flip fab -> fab -}


instance Bimap (,) where bimap f g (x,y) = (f x, g y)

bimap_lmap :: Bimap p => (x -> a) -> p x b -> p a b
bimap_lmap = (`bimap` (\b -> b))
bimap_rmap :: Bimap p => (x -> b) -> p a x -> p a b
bimap_rmap = bimap (\a -> a)

instance Bimap E where
  bimap f g = \case
    L a -> L (f a)
    R b -> R (g b)
