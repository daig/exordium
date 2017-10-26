module Where (Where(..), module X) where
import InLR as X
import Bimap as X
import Biempty as X

data Where a b = Here a | There b | Nowhere
instance Bimap Where where
  bimap f g = \case
    Here a -> Here (f a)
    There b -> There (g b)
    Nowhere -> Nowhere

instance InLR Where where
  inL = Here
  inR = There
instance Biempty Where where biempty = Nowhere
