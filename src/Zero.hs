module Zero (Zero,zero,module X) where
import Plus as X
import Def as X

import Bool
import Ord

type Zero a = (Plus a, Def a)
zero :: Zero a => a
zero = def
plusZero :: (Eq a, Zero a) => a -> Bool
plusZero a = (a + zero) == a && (zero + a) == a

