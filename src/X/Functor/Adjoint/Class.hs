module X.Functor.Adjoint.Class (module X.Functor.Adjoint.Class, module X) where
import X.Functor.Monad as X
import X.Functor.Comonad as X
{-import X.Type.O as X-}
import X.Arrow.Promap as X
{-import X.Star as X-}
{-import X.Optic.Grate as X (FZip(..))-}
import X.Functor.Indexed as X
{-import X.Optic.Review-}
{-import X.Optic.View-}
{-import X.Arrow.Promap-}

{-class (Monad (f `O` g), Comonad (g `O` f),Map f, Fold_ f, Indexed g) => f -| g | f -> g, g -> f where-}
  {-{-adjuncted :: Costar f ~~~= Star g-}-}
  {-{-adjuncted = isoP (Star < leftAdjunct < runCostar) (Costar < rightAdjunct < runStar)-}-}
  {-leftAdjunct :: (f a ->   b) ->   a -> g b-}
  {-{-leftAdjunct = runStar < view adjuncted < Costar-}-}
  {-rightAdjunct :: (a   -> g b) -> f a ->   b-}
  {-{-rightAdjunct = runCostar < review adjuncted < Star-}-}

