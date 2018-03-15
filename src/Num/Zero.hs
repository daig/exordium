module Num.Zero where
import GHC.Natural
import GHC.Integer
import Bool
import Int
import Word
import {-# source #-} Maybe

class Zero x where zero :: x

instance Zero Natural where zero = 0
instance Zero Int where zero = 0
instance Zero Integer where zero = 0
instance Zero Word   where zero = 0
instance Zero Bool where zero = F
instance Zero () where zero = ()
instance (Zero a, Zero b) => Zero (a,b) where zero = (zero,zero)
instance Zero (Maybe a) where zero = Nothing
