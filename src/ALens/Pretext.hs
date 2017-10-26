module ALens.Pretext
  (Pretext(..)
  ,module X) where
import Map as X

data Pretext p a b t = Pretext {runPretext :: forall f. Map f => p a (f b) -> f t}
