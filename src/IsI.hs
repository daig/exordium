module IsI (IsI(..), module X) where
import I as X
import Dimap as X
import Prelude (seq)
import Coerce

-- | Isomorphic to I
class (LinFoldMap f, Applicative f, Distributive f) => IsI f where
  taintedMap :: Map g => g a -> g (f a)
  taintedDot :: Dimap p => p a b -> p a (f b)
  untaintedDot :: Dimap p => p a (f b) -> p a b
  untaintedMap :: Map g => g (f a) -> g a
  taintedMap g = g `seq` map pure g
  taintedDot g = g `seq` postmap pure g
  untaintedMap g = g `seq` map fold_ g
  untaintedDot g = g `seq` postmap fold_ g

-- TODO: These instances should be generated automatically depending if f is a newtype or not

instance IsI I where
  taintedMap = map# pure
  taintedDot = postmap# pure
  untaintedDot = postmap# fold_
  untaintedMap = map# fold_
