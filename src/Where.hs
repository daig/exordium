module Where where
import Pure2
import Bimap
import Biempty

data Where a b = Here a | There b | Nowhere
instance Bimap Where where
  bimap f g = \case
    Here a -> Here (f a)
    There b -> There (g b)
    Nowhere -> Nowhere
instance Pure2 Where where
  pureL = Here
  pureR = There
instance Biempty Where where biempty = Nowhere
