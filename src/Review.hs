module Review (type (|~), module X) where
import Prism as X
import Choice as X
import Bimap as X

type t |~ b = forall p. (Bimap p, Choice p) => p b b -> p t t
