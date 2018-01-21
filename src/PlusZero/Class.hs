module PlusZero.Class (module PlusZero.Class, module X) where
import Plus.Class as X
import Zero.Class as X
import Int.Type

-- | zero + a = a + zero = a
class (Plus a, Zero a) => PlusZero a

instance PlusZero (a -> a)
instance (PlusZero a, PlusZero b) => PlusZero (a,b)
instance PlusZero Int
