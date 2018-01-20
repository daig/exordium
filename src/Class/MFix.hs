{-# language MagicHash #-}
module Class.MFix (module Class.MFix, module X) where
import Class.Monad as X

class Monad m => MFix m where
  {-# minimal mfix #-}
  mfix :: (a -> m a) -> m a
  mfix# :: (a -> m a) -> m a
  mfix# = mfix
