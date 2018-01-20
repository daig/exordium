module Class.PlusZero (module Class.PlusZero, module X) where
import Class.Plus as X
import Class.Zero as X
import Type.Int

-- | zero + a = a + zero = a
class (Plus a, Zero a) => PlusZero a

instance PlusZero (a -> a)
instance (PlusZero a, PlusZero b) => PlusZero (a,b)
instance PlusZero Int
