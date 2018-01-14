module Adjoint
  (type (-|)(..)
  ,pureDefault, extractDefault
  ,module X) where
import Monad as X
import Comonad as X hiding (mapDefault)
import O as X
import Isos as X
import Star as X
import Costar as X
import Indexed as X hiding (mapDefault)
import Coerce
import Fun
import AFold
import AReview

class (Monad (f `O` g), Comonad (g `O` f),FoldMap_ f, Indexed g) => f -| g | f -> g, g -> f where
  adjuncted :: Costar f ~~~= Star g
  adjuncted = isoP (Star < leftAdjunct < runCostar) (Costar < rightAdjunct < runStar)
  leftAdjunct :: (f a ->   b) ->   a -> g b
  leftAdjunct = runStar < view adjuncted < Costar
  rightAdjunct :: (a   -> g b) -> f a ->   b
  rightAdjunct = runCostar < review adjuncted < Star

pureDefault :: f -| g => a -> O g f a
pureDefault = O < leftAdjunct id

extractDefault :: f -| g => O f g a -> a
extractDefault = rightAdjunct id < unO

tabulateDefault :: f -| g => (f () -> b) -> g b
tabulateDefault f = leftAdjunct f ()

extractFDefault :: f -| g => f a -> a
extractFDefault = splitF > \case {(a,_) -> a}

splitF :: f -| g => f a -> (a,f ())
splitF = rightAdjunct ((`leftAdjunct` ()) < (,))
