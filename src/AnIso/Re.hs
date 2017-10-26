module AnIso.Re (Re(..),module X) where
import Dimap as X
import Fun

newtype Re p s t a b = Re {runRe :: p b a -> p t s}
instance Dimap p => Dimap (Re p s t) where dimap f g (Re r) = Re (r < dimap g f)
