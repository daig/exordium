module MutVar.Prim (module X) where
import GHC.Prim as X
  (MutVar#, State#, Int#
  ,newMutVar#, readMutVar#, writeMutVar# ,atomicModifyMutVar#
  ,sameMutVar# ,casMutVar# {-TODO: document compare-and-swap - possible improvement in atomic-primops-})

