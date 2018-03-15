module Num.FromNatural (FromNatural(..), module X) where
import Num.Rg as X
import Num.Add0 as X
import Num.One as X

-- | A "Rig": Ring without negatives
class (Rg r, Add0 r, One r) => FromNatural r where
  fromNatural :: Natural -> r
  fromNatural = (`scale0` one)
