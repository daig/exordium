module X.Num.Rg (Rg, module X) where
import X.Num.Add as X
import X.Num.Mul as X
import GHC.Integer
import X.Type.Int
import X.Type.Word
import X.Data.Bool

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
