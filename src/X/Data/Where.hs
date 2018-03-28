module X.Data.Where where
import X.Functor.Pure as X
import X.Functor.Swap as X
import X.Functor.Empty as X
import X.Num.Zero as X


data Where a b = Nowhere | Here a | There b

instance Pure (Where a) where pure = There
instance Empty (Where a) where empty = Nowhere
instance Map (Where a) where
  map f = \case
    Nowhere -> Nowhere
    Here a -> Here a
    There b -> There (f b)
instance Zero (Where a b) where zero = Nowhere
instance Swap Where where
  swap = \case
    Here a -> There a
    There b -> Here b
    Nowhere -> Nowhere
