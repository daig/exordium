module APrism
  (type (+~.), type (+~~.)
  ,withPrism
  ,module X) where
import APrism.Market as X (Market,Market')
import APrism.Market
import I as X

type (s +~. a) b t = Market a b a b -> Market a b s t
type s +~~. a = Market' a a a -> Market' a s s

withPrism :: (s +~. a) b t -> ((s -> E t a) -> (b -> t) -> r) -> r
withPrism l f = case l (Market (\x -> x) R) of Market g h -> f h g
