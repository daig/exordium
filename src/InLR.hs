module InLR where
import Bimap

class Bimap p => InLR p where
  inL :: a -> p a b
  inR :: b -> p a b
-- bimap f _ (inL a) = inL (f a)
-- bimap _ g (inR b) = inR (g a)
