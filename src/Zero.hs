module Zero where
import {-# source #-} Maybe
import Int

class Zero a where zero :: a

instance Zero (a -> a) where zero = \a -> a
instance (Zero a, Zero b) => Zero (a,b) where zero = (zero,zero)
instance Zero Int where zero = 0
instance Zero (Maybe a) where zero = Nothing
instance Zero [a] where zero = []
instance Zero () where zero = ()

