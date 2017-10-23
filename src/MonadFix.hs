module MonadFix (MonadFix(..),module X) where
import Monad as X

class Monad m => MonadFix m where
  {-# minimal mfix #-}
  mfix :: (a -> m a) -> m a
  mfix# :: (a -> m a) -> m a
  mfix# = mfix

fix :: (a -> a) -> a
fix f = let x = f x in x
