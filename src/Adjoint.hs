module Adjoint
  (type (-|)(..)
  ,pureDefault, extractDefault
  ,module X) where
import Monad as X
import Comonad as X
import O as X
import Iso as X
import Star as X
import Costar as X
import Coerce
import Fun
import AFold
import AReview

class (Monad (f `O` g), Comonad (g `O` f)) => f -| g | f -> g, g -> f where
  adjoint :: Costar f =::~ Star g
  adjoint = isoP (Star < leftAdjunct < runCostar) (Costar < rightAdjunct < runStar)
  leftAdjunct :: (f a ->   b) ->   a -> g b
  leftAdjunct = runStar < view adjoint < Costar
  rightAdjunct :: (a   -> g b) -> f a ->   b
  rightAdjunct = runCostar < review adjoint < Star

pureDefault :: f -| g => a -> O g f a
pureDefault = O < leftAdjunct id

extractDefault :: f -| g => O f g a -> a
extractDefault = rightAdjunct id < unO
