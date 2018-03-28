{-# language MagicHash #-}
module X.Prim.MutVar
  (MutVar#, State#, Int#
  ,newMutVar#, readMutVar#, writeMutVar# ,atomicModifyMutVar#
  ,sameMutVar# ,casMutVar# ) where
import GHC.Prim
{-TODO: document compare-and-swap - possible improvement in atomic-primops-}
