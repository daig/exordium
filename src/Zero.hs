module Zero where
import {-# source #-} Maybe
import Int

class Zero a where zero :: a

instance Zero (a -> a) where zero = \a -> a
instance (Zero a, Zero b) => Zero (a,b) where zero = (zero,zero)
instance Zero Int where zero = 0
{-instance Zero Integer where zero = 0-}
{-instance Zero Word   where zero = 0-}
{-instance Zero Word8  where zero = 0-}
{-instance Zero Word16 where zero = 0-}
{-instance Zero Word32 where zero = 0-}
{-instance Zero Word64 where zero = 0-}
{-instance Zero Bool where zero = False-}

instance Zero (Maybe a) where zero = Nothing
instance Zero [a] where zero = []
instance Zero () where zero = ()