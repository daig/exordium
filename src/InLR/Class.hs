module InLR.Class (module InLR.Class, module X) where
import Bimap.Class as X
import Where.Type

class Bimap p => InLR p where
  inL :: a -> p a b
  inR :: b -> p a b
-- bimap f _ (inL a) = inL (f a)
-- bimap _ g (inR b) = inR (g a)

instance InLR Where where
  inL = Here
  inR = There
