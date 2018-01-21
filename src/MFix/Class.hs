{-# language MagicHash #-}
module MFix.Class (module MFix.Class, module X) where
import Monad.Class as X

class Monad m => MFix m where
  {-# minimal mfix #-}
  mfix :: (a -> m a) -> m a
  mfix# :: (a -> m a) -> m a
  mfix# = mfix
