{-# language MagicHash #-}
module Type.IO (module X) where
import GHC.Types as X (IO(..))
import Control.Monad.ST as X (RealWorld) -- fix
import GHC.Prim as X (State#)
