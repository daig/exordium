module Bitraverse0.Class (module Bitraverse0.Class, module X) where
import Bitraverse.Class as X
import BifoldMap0.Class as X
import Bind.Class
import Tuple
import List

class (BifoldMap0 t, Bitraverse t) => Bitraverse0 t where
--  {-# minimal traverse | cocollect | sequence #-}
  bitraverse0 :: Pure f => (a -> f a') -> (b -> f b') -> t a b -> f (t a' b')
  bitraverse0 f g t = bisequence0 (bimap f g t)
  {-cocollect :: Applicative f => (t a -> b) -> t (f a) -> f b-}
  {-cocollect tab tfa = map tab (sequence tfa)-}
  bisequence0 :: Pure f => t (f a) (f b) -> f (t a b)
  bisequence0 = bitraverse0 (\fa -> fa) (\fb -> fb)
