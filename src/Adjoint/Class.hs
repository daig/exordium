module Adjoint.Class (module Adjoint.Class, module X) where
import Monad.Class as X
import Comonad.Class as X hiding (mapDefault)
import Type.O as X
import Utils.Dimap as X
import Type.Star as X
import Costar as X
import Indexed as X hiding (mapDefault)
import Coerce
import Fun
import AFold
import Prisms (review)
import Utils.Dimap

class (Monad (f `O` g), Comonad (g `O` f),FoldMap_ f, Indexed g) => f -| g | f -> g, g -> f where
  adjuncted :: Costar f ~~~= Star g
  adjuncted = isoP (Star < leftAdjunct < runCostar) (Costar < rightAdjunct < runStar)
  leftAdjunct :: (f a ->   b) ->   a -> g b
  leftAdjunct = runStar < view adjuncted < Costar
  rightAdjunct :: (a   -> g b) -> f a ->   b
  rightAdjunct = runCostar < review adjuncted < Star
