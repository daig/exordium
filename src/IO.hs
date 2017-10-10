{-# OPTIONS_GHC -Wno-orphans #-}
{-# language UnboxedTuples #-}
module IO where
{-import IO.Prim-}
import GHC.Types as X (IO(..))
import Monad

instance Map IO where
  map f (IO k) = IO (\s -> case k s of {(# s', a #) -> (# s', f a #)})
  constMap b (IO k) = IO (\s -> case k s of {(# s', _ #) -> (# s', b #)})
instance Bind IO where join (IO k) = IO (\s -> case k s of {(# s', IO k' #) -> k' s'})
instance Apply IO where (|@|) = apDefault
instance Pure IO where pure a = IO (\s -> (# s , a #))
  
