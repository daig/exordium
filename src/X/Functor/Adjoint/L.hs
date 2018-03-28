{-# language UndecidableSuperClasses #-}
module X.Functor.Adjoint.L (module X) where
import X.Functor.Monad as X
import X.Functor.Comonad as X hiding (mapDefault)
import X.Type.O as X
import X.Arrow.Promap as X
{-import X.Star as X-}
import X.Optic.Grate as X (FZip(..))
import X.Functor.Indexed as X hiding (mapDefault)
import X.Cast
import X.Optic.Review
import X.Optic.View
import X.Arrow.Promap

{-class (Monad (f `O` RightAdjunct f)-}
      {-,Comonad (RightAdjunct f `O` f)-}
      {-,RightAdjoint (RightAdjunct f)-}
      {-,LeftAdjunct (RightAdjunct f) ~ f-}
      {-,Comonad f) -- TODO: should this actually be Traverse_ ? Why not?-}
   {-=> LeftAdjoint f where-}
  {-type RightAdjunct f = (g :: * -> *) | g -> f-}
  {-leftAdjunct :: (f a -> b) -> a -> RightAdjunct f b-}

{-class (Monad (LeftAdjunct g `O` g)-}
      {-,Comonad (g `O` LeftAdjunct g)-}
      {-,LeftAdjoint (LeftAdjunct g)-}
      {-,RightAdjunct (LeftAdjunct g) ~ g-}
      {-,Indexed g, Ix g ~ LeftAdjunct g ()) => RightAdjoint (g :: * -> *) where-}
  {-type LeftAdjunct g = (f :: * -> *) | f -> g-}
  {-rightAdjunct :: (a -> g b) -> LeftAdjunct g a -> b-}

{-type f -| g = (LeftAdjoint f, RightAdjunct f ~ g, RightAdjoint g, LeftAdjunct g ~ f)-}

{-instance LeftAdjoint ((,) x) where-}
  {-type RightAdjunct ((,) x) = (->) x-}
  {-{-leftAdjunct xab a = -}-}
{-instance RightAdjoint ((->) x) where-}
  {-type LeftAdjunct ((->) x) = (,) x-}

{-instance Indexed ((->) x) where-}
  {-type Ix ((->) x) = x-}
