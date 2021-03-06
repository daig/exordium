{-# language MagicHash #-}
module X.Num.One' (module X.Num.One', module X) where
import X.Num.One as X
import X.Data.Bool as X
import X.Data.Struct.Natural
import GHC.Integer
import X.Type.Int
import X.Type.Word
import X.Stock.Eq

class One a => One' a where
  one' :: a -> Bool
  default one' :: Eq# a => a -> Bool
  one' = eq# one 

pattern One :: One' a => a
pattern One <- (one' -> T) where One = one

instance One' Natural
instance One' Bool
instance One' Int
instance One' Integer
instance One' Word
