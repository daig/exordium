module APrism
  (type (+~.), type (+~~.)
  ,module X) where

type (s +~. a) b t = Market a b a (I b) -> Market a b s (I t)
type s +~~. a = Market' a a (I a) -> Market' a s (I s)
