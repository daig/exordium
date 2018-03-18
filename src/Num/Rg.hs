module Num.Rg (Rg, module X) where
import Num.Add as X
import Num.Mul as X
import GHC.Integer
import Type.Int
import Type.Word
import ADT.Bool

-- | Near Semiring. Ie a "Ring" without the Identity and Negation
-- a(b + c) = ab + ac
-- (a + b)c = ac + bc
-- so that: (x+y)(s+t) := xs + ys + xt + yt = xs + xt + ys + yt
class (Add r, Mul r) => Rg r


instance Rg Natural
instance Rg Integer
instance Rg Int
instance Rg Word
instance Rg Bool
