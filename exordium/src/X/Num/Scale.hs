module X.Num.Scale (Scale(..), module X) where
import X.Num.Rg as X

-- | A near semiring (bi)module X..
--
--   (r*s)a = r(sa)
--
--   r(as) = (ra)s
--
--   (r+s)a = ra + sa
--
--   r(a+b) = ra + rb
class (Rg r, Add a) => Scale r a where scale :: r -> a -> a
