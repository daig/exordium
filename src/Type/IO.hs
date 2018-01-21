{-# language MagicHash #-}
module Type.IO (module X) where
import GHC.Types as X (IO(..))
import Control.Monad.ST as X (RealWorld) -- fix
import GHC.Prim as X (State#)

{-import Traverse.Class_ as X-}
{-import Monad.Class-}

{-instance MapIso IO where mapIso = map_mapIso-}
{-instance Map IO where-}
  {-map f (IO k) = IO (\s -> case k s of {(# s', a #) -> (# s', f a #)})-}
  {-b !@ IO k = IO (\s -> case k s of {(# s', _ #) -> (# s', b #)})-}
{-instance Bind IO where join (IO k) = IO (\s -> case k s of {(# s', IO k' #) -> k' s'})-}
{-instance Apply IO where (|$|) = apDefault-}
{-instance Pure IO where pure a = IO (\s -> (# s , a #))-}

{-instance Traverse_ IO where-}
  {-traverse_ f (IO io) = IO (\s -> -}
