module Review (type (|~), type (|~.), module X) where
import K as X

type t |~ b = forall p f. (IsKK p, IsI f) => p b (f b) -> p t (f t)
type t |~. b = KK b (I b) -> KK t (I t)
