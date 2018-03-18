module Functor.Adjoint.Class (module Functor.Adjoint.Class, module X) where
import Functor.Monad as X
import Functor.Comonad as X hiding (mapDefault)
import Type.O as X
import Arrow.Promap as X
{-import Star as X-}
{-import Optic.Grate as X (FZip(..))-}
import Functor.Indexed as X hiding (mapDefault)
import Coerce
{-import Optic.Review-}
{-import Optic.View-}
{-import Arrow.Promap-}

class (Monad (f `O` g), Comonad (g `O` f),Map f, Fold_ f, Indexed g) => f -| g | f -> g, g -> f where
  {-adjuncted :: Costar f ~~~= Star g-}
  {-adjuncted = isoP (Star < leftAdjunct < runCostar) (Costar < rightAdjunct < runStar)-}
  leftAdjunct :: (f a ->   b) ->   a -> g b
  {-leftAdjunct = runStar < view adjuncted < Costar-}
  rightAdjunct :: (a   -> g b) -> f a ->   b
  {-rightAdjunct = runCostar < review adjuncted < Star-}

