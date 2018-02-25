module Bitraverse.Class (module Bitraverse.Class, module X) where
import Bitraverse1.Internal
import BifoldMap.Class as X
import Bimap.Class as X
import Applicative.Class as X
import Bind.Class
import Tuple
import List
import {-# source #-} O

class (Bimap t,BifoldMap t) => Bitraverse t where
--  {-# minimal traverse | cocollect | sequence #-}
  bitraverse :: Applicative f => (a -> f a') -> (b -> f b') -> t a b -> f (t a' b')
  bitraverse f g t = bisequence (bimap f g t)
  {-cocollect :: Applicative f => (t a -> b) -> t (f a) -> f b-}
  {-cocollect tab tfa = map tab (sequence tfa)-}
  bisequence :: Applicative f => t (f a) (f b) -> f (t a b)
  bisequence = bitraverse (\fa -> fa) (\fb -> fb)

bitraverse_bifoldMap :: (PlusZero m, Bitraverse t) => (a -> m) -> (b -> m) -> t a b -> m
bitraverse_bifoldMap f g t = thesek'biextract (bitraverse (\x -> ThisK (f x)) (\y -> ThatK (g y)) t)

instance Bitraverse (,) where bitraverse f g (a,b) = (,) `map` f a `ap` g b

{-bitraverse_bifoldMap :: (Applicative m, Bitraverse t) => (a -> m) -> (b -> m) -> t a b -> m-}
{-bitraverse_bifoldMap f g t = thesek'biextract (bitraverse1 (\x -> ThisK (f x)) (\y -> ThatK (g y)) t)-}
