module Power.Laws where
import Power
import Negate as X
import Recip as X
import Bool

zeroOne :: forall a b. (Zero a, One b, Power a b, Eq b) => b -> Bool
zeroOne b = (zero @a ^ b) == one
oneOne :: forall a b. (Zero a, One b, Power a b, Eq b) => a -> Bool
oneOne a = (a ^ one) == one @b
distrib :: forall a b. (Zero a, One b, Power a b, Eq b) => a -> a -> b -> b -> Bool
distrib a b x y = ((a + b) ^ (x * y)) == ((a ^ x) * (a ^ y) * (b ^ x) * (b ^ y))
negateRecip :: forall a b. (Negate a, Recip b, Power a b, Eq b) => a -> b -> Bool
negateRecip a b = (negate a ^ b) == (a ^ (recip b))
