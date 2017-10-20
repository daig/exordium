module AnIso
  (type (=~.), type (=~~.)
  ,withIso, under
  ,module X) where
import AnIso.Exchange as X (Exchange)
import I as X
import AnIso.Exchange
import Coerce (postmap#)

type (s =~. a) b t = Exchange a b a (I b) -> Exchange a b s (I t)
type (s =~~. a) b t = Exchange a a a (I a) -> Exchange a a s (I s)

withIso :: (s =~. a) b t -> ((s -> a) -> (b -> t) -> r) -> r
withIso ai k = case ai (Exchange (\x -> x) I) of {Exchange sa bt -> k sa ((\(I x) -> x) `postmap#` bt)}

under :: (s =~. a) b t -> (t -> s) -> b -> a
under k = withIso k (\sa bt ts x -> sa (ts (bt x)))
