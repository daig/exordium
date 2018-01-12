module Bimap where
import Fun

-- | Independently Map each on both sides
class Bimap p where
  {-# minimal bimap | lmap,rmap #-}
  bimap :: (x -> a) -> (y -> b) -> p x y -> p a b
  bimap f g p = rmap g (lmap f p)
  lmap :: (x -> a) -> p x b -> p a b
  lmap f = bimap f (\b -> b)
  rmap :: (x -> b) -> p a x -> p a b
  rmap = bimap (\a -> a)
  bothmap :: (x -> a) -> p x x -> p a a
  bothmap f = bimap f f


(<@>) :: Bimap p => (x -> a) -> (y -> b) -> p x y -> p a b
($@.) :: Bimap p => (x -> a) -> p x b -> p a b
($.@) :: Bimap p => (x -> b) -> p a x -> p a b
(&@)  :: Bimap p => (a -> b) -> p a a -> p b b
(<@>) = bimap
(&@) f = bimap f f
($@.) = lmap -- TODO: unsatisfying names
($.@) = rmap --

instance Bimap (,) where bimap f g (x,y) = (f x, g y)
