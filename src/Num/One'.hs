{-# language MagicHash #-}
module Num.One' (module Num.One', module X) where
import Num.One as X
import ADT.Bool as X
import Struct.Natural
import GHC.Integer
import Type.Int
import Type.Word
import Stock.Eq
import qualified Prelude as P

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
