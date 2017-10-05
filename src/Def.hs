module Def where
import Coerce (fromInteger)
import Types

class Def a where def :: a

instance Def Integer where def = 0
instance Def Int   where def = 0
instance Def Int8  where def = 0
instance Def Int16 where def = 0
instance Def Int32 where def = 0
instance Def Int64 where def = 0
instance Def Word   where def = 0
instance Def Word8  where def = 0
instance Def Word16 where def = 0
instance Def Word32 where def = 0
instance Def Word64 where def = 0
instance Def Bool where def = False
