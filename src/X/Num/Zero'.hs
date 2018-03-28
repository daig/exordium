{-# language MagicHash #-}
module X.Num.Zero' (module X.Num.Zero', module X) where
import X.Num.Zero as X
import X.Data.Bool as X
import X.Data.Struct.Natural
import GHC.Integer
import X.Type.Int
import X.Type.Word
import X.Stock.Eq
import qualified Prelude as P


class Zero a => Zero' a where
  zero' :: a -> Bool
  default zero' :: Eq# a => a -> Bool
  zero' = eq# zero

pattern Zero :: Zero' a => a
pattern Zero <- (zero' -> T) where Zero = zero


instance Zero' Natural
instance Zero' Bool
instance Zero' Int
instance Zero' Integer
instance Zero' Word
