module Comonad
  (Comonad(..)
  ,mapDefault
  ,module X) where
import Coapplicative as X
import Duplicate as X
import Fun

-- | extend extract = id
-- extract < extend f = f
class (Coapplicative w,Duplicate w) => Comonad w

mapDefault :: Comonad w => (a -> b) -> w a -> w b
mapDefault f = extend (f < fold_)
