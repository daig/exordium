module PlusZero (PlusZero,module X) where
import Plus as X
import Zero as X

-- | zero + a = a + zero = a
class (Plus a, Zero a) => PlusZero a

instance PlusZero (a -> a)
instance (PlusZero a, PlusZero b) => PlusZero (a,b)
