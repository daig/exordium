module APrism
  (type (+~.), type (+~~.)
  ,module X) where
import APrism.Market as X (Market,Market')
import I as X

type (s +~. a) b t = Market a b a (I b) -> Market a b s (I t)
type s +~~. a = Market' a a (I a) -> Market' a s (I s)
