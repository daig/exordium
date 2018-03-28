module X.Num.FromNatural (FromNatural(..), module X) where
import X.Num.Rg as X
import X.Num.Add0 as X
import X.Num.One as X
import X.Struct.Natural
import X.Type.Int
import qualified Prelude as P

-- | A "Rig": Ring without negatives
class (Rg r, Add0 r, One r) => FromNatural r where
  fromNatural :: Natural -> r
  fromNatural = (`scale0` one)

instance FromNatural Natural where fromNatural n = n
instance FromNatural Int where fromNatural = P.fromIntegral
