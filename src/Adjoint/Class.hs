module Adjoint.Class (module Adjoint.Class, module X) where
import Monad as X
import Comonad as X hiding (mapDefault)
import O as X
import Map.Di as X
import Star.Type as X
import Costar as X
import Indexed as X hiding (mapDefault)
import Coerce
import Optic.Review
import Optic.View
import Map.Di

class (Monad (f `O` g), Comonad (g `O` f),FoldMap_ f, Indexed g) => f -| g | f -> g, g -> f where
  adjuncted :: Costar f ~~~= Star g
  adjuncted = isoP (Star < leftAdjunct < runCostar) (Costar < rightAdjunct < runStar)
  leftAdjunct :: (f a ->   b) ->   a -> g b
  leftAdjunct = runStar < view adjuncted < Costar
  rightAdjunct :: (a   -> g b) -> f a ->   b
  rightAdjunct = runCostar < review adjuncted < Star

