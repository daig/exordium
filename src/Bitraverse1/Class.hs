module Bitraverse1.Class (module Bitraverse1.Class, module X) where
{-import Bitraverse1.Internal-}
import Bitraverse.Class as X
import BifoldMap1.Class as X
import Bind.Class
import Coerce
import Tuple
import List

class (BifoldMap1 t, Bitraverse t) => Bitraverse1 t where
--  {-# minimal traverse | cocollect | sequence #-}
  bitraverse1 :: Apply f => (a -> f a') -> (b -> f b') -> t a b -> f (t a' b')
  bitraverse1 f g t = bisequence1 (bimap f g t)
  {-cocollect :: Applicative f => (t a -> b) -> t (f a) -> f b-}
  {-cocollect tab tfa = map tab (sequence tfa)-}
  bisequence1 :: Apply f => t (f a) (f b) -> f (t a b)
  bisequence1 = bitraverse1 (\fa -> fa) (\fb -> fb)

{-bitraverse1_bifoldMap1 :: (Plus m, Bitraverse1 t) => (a -> m) -> (b -> m) -> t a b -> m-}
{-bitraverse1_bifoldMap1 f g t = thesek'biextract (bitraverse1 (\x -> ThisK (f x)) (\y -> ThatK (g y)) t)-}
    

{-foldMapDefault :: (Bitraverse1 t, Zero m) => (a -> m) -> t a -> m-}
{-foldMapDefault f t = case traverse (\x -> K (f x)) t of {K m -> m}-}

instance Bitraverse1 (,) where bitraverse1 f g (a,b) = (,) `map` f a `ap` g b
