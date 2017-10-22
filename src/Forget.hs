module Forget where
import Plus as X
import Strong as X
import Map as X
import Witness

newtype Forget r a b = Forget (a -> r)
instance Strong (Forget r) where
  first (Forget z) = Forget (\(a,_) -> z a)
instance Dimap (Forget r) where
  dimap f _ (Forget z) = Forget (premap f z)
instance ComapL (Forget r) where comapl f (Forget z) = Forget (premap f z)
instance MapR (Forget r) where mapr _ (Forget z) = Forget z; mapDict = W
instance Map (Forget r a) where map = mapr
