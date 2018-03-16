module Num.One' (module Num.One', module X) where
import Num.One as X
import Bool as X
import GHC.Natural
import GHC.Integer
import Int
import Word
import Num.Eq
import qualified Prelude as P

class One a => One' a where
  one' :: a -> Bool
  default one' :: Eq a => a -> Bool
  one' = (one ==)

pattern One :: One' a => a
pattern One <- (one' -> T) where One = one

instance One' Natural
instance One' Bool
instance One' Int
instance One' Integer
instance One' Word
