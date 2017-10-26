module Biapply where
import Bimap

class Bimap p => Biapply p where
  biapply :: p (x -> a) (y -> b) -> p x y -> p a b
  constBiapply :: p x y -> p a b -> p a b
  constBiapply = \p -> biapply (bimap (\_ a -> a) (\_ b -> b) p)
