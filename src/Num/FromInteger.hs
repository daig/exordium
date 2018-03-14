module Num.FromInteger (FromInteger(..), module X) where
import Num.FromNatural as X
import Num.Sub as X

-- | A Ring
class (FromNatural r, Sub r) => FromInteger r where
  fromInteger :: Integer -> r
  fromInteger = (`scalei` one)
