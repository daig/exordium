module AnIso
  (type (=~.), type (=~~.)
  ,module X) where
import AnIso.Exchange as X (Exchange)
import I as X

type (s =~. a) b t = Exchange a b a (I b) -> Exchange a b s (I t)
type (s =~~. a) b t = Exchange a a a (I a) -> Exchange a a s (I s)
