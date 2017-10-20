module Review (type (|~), module X) where
import Prism as X
import IsI as X
import Choice as X
import Bimap as X

type t |~ b = forall p f. (Bimap p, Choice p, IsI f) => p b (f b) -> p t (f t)
