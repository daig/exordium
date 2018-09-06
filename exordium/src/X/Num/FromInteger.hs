module X.Num.FromInteger (module X.Num.FromInteger, module X) where
import X.Num.FromNatural as X
import X.Num.Negate as X

-- | A Ring
class (FromNatural r, Negate r) => FromInteger r where
  fromInteger :: Integer -> r
  fromInteger = (`scalei` one)
