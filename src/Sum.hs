module Sum (E(..), module X) where
import Assoc as X
import Either as X
import Bimap as X
import InLR as X
import Swap as X
import Map as X
import Sum.Type as X


instance Bimap E where bimap f g = \case {L a -> L (f a); R b -> R (g b)}
instance Assoc E where
  assoc = \case
    L a -> L (L a)
    R (L b) -> L (R b)
    R (R c) -> R c
  assoc' = \case
    L (L a) -> L a
    L (R b) -> R (L b)
    R c -> R (R c)
instance Either E where
  either f g = \case {L a -> f a; R b -> g b}
instance InLR E where
  inL = L
  inR = R
instance Swap E where swap = \case {L a -> R a; R b -> L b}
instance Map (E a) where map = rmap
instance MapIso (E a) where mapIso _ = rmap
