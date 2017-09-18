module Where where
import LRPure
import Bimap
import Biempty

data Where a b = Here a | There b | Nowhere
instance Bimap Where where
  bimap f g = \case
    Here a -> Here (f a)
    There b -> There (g b)
    Nowhere -> Nowhere
instance LRPure Where where
  inL = Here
  inR = There
instance Biempty Where where biempty = Nowhere
