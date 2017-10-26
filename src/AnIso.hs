module AnIso
  (type (=~.), type (=~~.)
  ,withIso, under
  ,module X) where
import Iso as X
import AnIso.Exchange as X (Exchange)
import I as X
import AnIso.Exchange

type (s =~. a) b t = Exchange a b a b -> Exchange a b s t
type (s =~~. a) b t = Exchange a a a a -> Exchange a a s s

withIso :: (s =~. a) b t -> ((s -> a) -> (b -> t) -> r) -> r
withIso ai k = case ai (Exchange (\x -> x) (\x -> x)) of {Exchange sa bt -> k sa bt}

under :: (s =~. a) b t -> (t -> s) -> b -> a
under k = withIso k (\sa bt ts x -> sa (ts (bt x)))
