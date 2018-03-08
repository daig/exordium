module InLR (module InLR, module X) where
import Map.Bi as X

class Bimap p => InLR p where
  inL :: a -> p a b
  inR :: b -> p a b
-- bimap f _ (inL a) = inL (f a)
-- bimap _ g (inR b) = inR (g a)

