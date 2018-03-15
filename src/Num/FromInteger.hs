module Num.FromInteger (module Num.FromInteger, module X) where
import Num.FromNatural as X
import Num.Negate as X

-- | A Ring
class (FromNatural r, Negate r) => FromInteger r where
  fromInteger :: Integer -> r
  fromInteger = (`scalei` one)
