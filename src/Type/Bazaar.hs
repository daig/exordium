module Type.Bazaar where

newtype Bazaar c a b t = Bazaar {runBazaar :: forall f. c f => (a -> f b) -> f t}
