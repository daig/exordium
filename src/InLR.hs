module Class.InLR (module Class.InLR, module X) where
import Class.Bimap as X
import Type.Where

class Bimap p => InLR p where
  inL :: a -> p a b
  inR :: b -> p a b
-- bimap f _ (inL a) = inL (f a)
-- bimap _ g (inR b) = inR (g a)

instance InLR Where where
  inL = Here
  inR = There
