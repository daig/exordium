module X.Num.Zero where
import X.Struct.Natural
import GHC.Integer
import X.ADT.Bool
import X.Type.Int
import X.Type.Word
import {-# source #-} X.ADT.Maybe

class Zero x where zero :: x

instance Zero Natural where zero = 0
instance Zero Int where zero = 0
instance Zero Integer where zero = 0
instance Zero Word   where zero = 0
instance Zero Bool where zero = F
instance Zero () where zero = ()
instance (Zero a, Zero b) => Zero (a,b) where zero = (zero,zero)
instance Zero (Maybe a) where zero = Nothing
instance Zero [a] where zero = []
instance Zero (a -> a) where zero a = a
