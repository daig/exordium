module Bitraverse_.Class (module Bitraverse_.Class, module X) where
import Bitraverse0.Class as X
import Bitraverse1.Class as X
import BifoldMap_.Class as X
import Bind.Class
import Tuple
import List
import {-# source #-} O

class (BifoldMap_ t, Bitraverse0 t, Bitraverse1 t) => Bitraverse_ t where
--  {-# minimal traverse | cocollect | sequence #-}
  bitraverse_ :: Map f => (a -> f a') -> (b -> f b') -> t a b -> f (t a' b')
  bitraverse_ f g t = bisequence_ (bimap f g t)
  {-cocollect :: Applicative f => (t a -> b) -> t (f a) -> f b-}
  {-cocollect tab tfa = map tab (sequence tfa)-}
  bisequence_ :: Map f => t (f a) (f b) -> f (t a b)
  bisequence_ = bitraverse_ (\fa -> fa) (\fb -> fb)

{-foldMapDefault :: (Bitraverse_ t, Zero m) => (a -> m) -> t a -> m-}
{-foldMapDefault f t = case traverse (\x -> K (f x)) t of {K m -> m}-}
