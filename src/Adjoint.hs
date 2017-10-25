module Adjoint where
import Monad as X
import Comonad as X
import O as X

class (Monad (O f g), Comonad (O g f)) => f -| g where
  leftAdjunct :: (f a ->   b) ->   a -> g b
  rightAdjunt :: (a   -> g b) -> f a ->   b
