module Adjoint.Class (module Adjoint.Class, module X) where
import Monad as X
import Monad.Co as X hiding (mapDefault)
import O as X
import Map.Pro as X
import Star as X
import Star.Co as X
import Indexed as X hiding (mapDefault)
import Coerce
import Optic.Review
import Optic.View
import Map.Pro

class (Monad (f `O` g), Comonad (g `O` f),FoldMap_ f, Indexed g) => f -| g | f -> g, g -> f where
  adjuncted :: Costar f ~~~= Star g
  adjuncted = isoP (Star < leftAdjunct < runCostar) (Costar < rightAdjunct < runStar)
  leftAdjunct :: (f a ->   b) ->   a -> g b
  leftAdjunct = runStar < view adjuncted < Costar
  rightAdjunct :: (a   -> g b) -> f a ->   b
  rightAdjunct = runCostar < review adjuncted < Star

