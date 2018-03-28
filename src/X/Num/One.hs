{-# language MagicHash #-}
module X.Num.One where
import X.Data.Struct.Natural
import GHC.Integer
import X.Type.Word
import X.Data.Bool
import X.Type.Int

-- | one * a = a * one = a
class One m where one :: m

instance One Natural where one = 1
instance One Integer where one = 1
instance One Word where one = 1
instance One Int where one = 1
instance One () where one = ()
instance One Bool where one = T
